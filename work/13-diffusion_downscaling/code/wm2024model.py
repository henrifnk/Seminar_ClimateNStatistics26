# Copyright (c) 2022, NVIDIA CORPORATION & AFFILIATES. All rights reserved.
#
# This work is licensed under a Creative Commons
# Attribution-NonCommercial-ShareAlike 4.0 International License.
# You should have received a copy of the license along with this
# work. If not, see http://creativecommons.org/licenses/by-nc-sa/4.0/

# This file was modified by Robbie Watt (2024) for the purpose of downscaling
# climate data

"""Model architectures and preconditioning schemes used in the paper
"Elucidating the Design Space of Diffusion-Based Generative Models"."""

import numpy as np
import torch
from torch.nn.functional import silu

#----------------------------------------------------------------------------
# Unified routine for initializing weights and biases.

## they just matter when we want to initialize the weights in a different way
def weight_init(shape, mode, fan_in, fan_out):
    if mode == 'xavier_uniform': return np.sqrt(6 / (fan_in + fan_out)) * (torch.rand(*shape) * 2 - 1)
    if mode == 'xavier_normal':  return np.sqrt(2 / (fan_in + fan_out)) * torch.randn(*shape)
    if mode == 'kaiming_uniform': return np.sqrt(3 / fan_in) * (torch.rand(*shape) * 2 - 1)
    if mode == 'kaiming_normal':  return np.sqrt(1 / fan_in) * torch.randn(*shape)
    raise ValueError(f'Invalid init mode "{mode}"')

#----------------------------------------------------------------------------
# Fully-connected layer.

# just as torch..nn.Linear but with custom weight initialization
class Linear(torch.nn.Module):
    def __init__(self, in_features, out_features, bias=True, init_mode='kaiming_normal', init_weight=1, init_bias=0):
        super().__init__()
        # stores dimensions for reference later
        self.in_features = in_features
        self.out_features = out_features
        # initialization arguments are packaged into a dict, so they can be easily passed to the weight_init function
        init_kwargs = dict(mode=init_mode, fan_in=in_features, fan_out=out_features)
        # weights and biases are initialized using the weight_init function, and then wrapped in torch.nn.Parameter so they can be learned during training
        self.weight = torch.nn.Parameter(weight_init([out_features, in_features], **init_kwargs) * init_weight)
        self.bias = torch.nn.Parameter(weight_init([out_features], **init_kwargs) * init_bias) if bias else None

    def forward(self, x):
        # linear transformation of x using the weights matrix, which is transposed before
        x = x @ self.weight.to(x.dtype).t()
        # adding of a bias
        if self.bias is not None:
            x = x.add_(self.bias.to(x.dtype))
        return x

#----------------------------------------------------------------------------
# Convolutional layer with optional up/downsampling.

class Conv2d(torch.nn.Module):
    # parameters; input/output channel counts; kernel size; whether to upsample or downsample; whether to use bias; 
    # resampling filter for up/downsampling; 
    ## we do this to not lose detail when we downsample, later we do (1,1) * (1,1)T, so we get a matrix where it averages itself out to the right, down and down right
    ## to blur the picture before downsampling so we avoid artifacts (so image is not corrupted because we stupidly remove every 2nd pixel)
    # fused resample = True; then the convolution and resampling are performed in a single step for efficiency; otherwise, they are performed separately.
    # initialization arguments for weights and biases; these are passed to the weight_init function to initialize the weights and biases of the convolutional layer. The init_mode argument specifies the method of initialization (e.g., 'kaiming_normal'), while init_weight and init_bias specify scaling factors for the initialized weights and biases, respectively.
    # whether to fuse the resampling with the convolution; initialization arguments for weights and biases;
    def __init__(self,
        in_channels, out_channels, kernel, bias=True, up=False, down=False,
        resample_filter=[1,1], fused_resample=False, init_mode='kaiming_normal', init_weight=1, init_bias=0,
    ):
        assert not (up and down)
        super().__init__()
        self.in_channels = in_channels
        self.out_channels = out_channels
        self.up = up
        self.down = down
        self.fused_resample = fused_resample
        ## we calculate the number of connectons going in a single layer and ging out of a single layer (fan_in and fan_out)
        init_kwargs = dict(mode=init_mode, fan_in=in_channels*kernel*kernel, fan_out=out_channels*kernel*kernel)
        ## weights and biases are initialized using the weight_init function, and then wrapped in torch.nn.Parameter so they can be learned during training. If kernel is 0, then weight and bias are set to None, which means that this layer will not perform any convolution (i.e., it will be an identity layer).
        ### we have 4 arguments (out_channels, in_channels, kernel, kernel) because we want to initialize a weight matrix of the right shape for the convolutional layer; the shape of the weight matrix for a convolutional layer is (out_channels, in_channels, kernel_size, kernel_size), where out_channels is the number of output channels, in_channels is the number of input channels, and kernel_size is the size of the convolutional kernel (e.g., 3 for a 3x3 kernel).
        self.weight = torch.nn.Parameter(weight_init([out_channels, in_channels, kernel, kernel], **init_kwargs) * init_weight) if kernel else None
        self.bias = torch.nn.Parameter(weight_init([out_channels], **init_kwargs) * init_bias) if kernel and bias else None
        ## resample filter is converted from a list to a tensor 
        f = torch.as_tensor(resample_filter, dtype=torch.float32)
        ## .ger() takes the outer product; unsqueeze() adds dimension for the batch; then we add one more dimension for the channel so we get (out_channels=1, in_channels=1, height=2, width=2)
        ## then we square and normalize the filter so that it sums to 1; this is important for downsampling to avoid losing too much information, and for upsampling to avoid introducing artifacts
        ### f is now the blurring kernel
        f = f.ger(f).unsqueeze(0).unsqueeze(1) / f.sum().square()
        self.register_buffer('resample_filter', f if up or down else None)

    def forward(self, x):
        # convert weights, biases, and resampling filter to the same datatype as the input tensor x for consistency during computation. If weights or biases are None (i.e., if kernel is 0), they remain None.
        w = self.weight.to(x.dtype) if self.weight is not None else None
        b = self.bias.to(x.dtype) if self.bias is not None else None
        f = self.resample_filter.to(x.dtype) if self.resample_filter is not None else None
        ## now it is determined how many pixels are added on the edges for the weights matrix and the resampling filter
        ## f needs padding, because it would break at the edges of the image, so we add padding to make it work; w needs padding because it is a convolutional kernel, and we want to make sure that the output has the same spatial dimensions as the input (i.e., we want to use 'same' padding)
        w_pad = w.shape[-1] // 2 if w is not None else 0
        f_pad = (f.shape[-1] - 1) // 2 if f is not None else 0

        ## here we have the fast path where blur and convolution happen in one combined operation
        ### upsampling + blurring; then CNN trafo
        if self.fused_resample and self.up and w is not None:
            # upsampling (insert pixels) + blurring
            x = torch.nn.functional.conv_transpose2d(x, f.mul(4).tile([self.in_channels, 1, 1, 1]), groups=self.in_channels, stride=2, padding=max(f_pad - w_pad, 0))
            # CNN transformation; padding is applied to ensure that the output has the same spatial dimensions as the input
            ## we pad the zeros around the edges so we can apply the w and f filters to the edges of the image without breaking; we take the max of the two padding values to ensure that we have enough padding for both filters
            x = torch.nn.functional.conv2d(x, w, padding=max(w_pad - f_pad, 0))
        ## CNN 
        elif self.fused_resample and self.down and w is not None:
            ## first we extract featurs with the convolutional kernel; we pad so the resulting image has the same size es the input image
            ### as a consequence values at the border will be pulled towards 0 after conv
            x = torch.nn.functional.conv2d(x, w, padding=w_pad+f_pad)
            ## now we downsample and blur in one step; stride=2 means we take every 2nd pixel, so we downsample by a factor of 2; the resampling filter f is applied to blur the image before downsampling to avoid artifacts
            x = torch.nn.functional.conv2d(x, f.tile([self.out_channels, 1, 1, 1]), groups=self.out_channels, stride=2)
       ## unfused path where convolution and resampling are performed separately; this is less efficient but more flexible, as it allows for different configurations of convolution and resampling (e.g., using a different resampling filter or performing convolution without resampling)
        else:
            ## upsampling of small pictures; we multiply f by 4 so the inserting of zeros and averaging with them is compensated; group=channel so every channel is upsampled independently; stride 2 and zeros added around the edhes
            if self.up:
                x = torch.nn.functional.conv_transpose2d(x, f.mul(4).tile([self.in_channels, 1, 1, 1]), groups=self.in_channels, stride=2, padding=f_pad)
            ## downsampling of large pictures; we first blur the image with the resampling filter f to avoid artifacts, and then we downsample by taking every 2nd pixel; group=channel so every channel is downsampled independently; padding is applied to ensure that the output has the same spatial dimensions as the input
            if self.down:
                x = torch.nn.functional.conv2d(x, f.tile([self.in_channels, 1, 1, 1]), groups=self.in_channels, stride=2, padding=f_pad)
            ## we do the CNN layer around the image; the result at each position is one value per output channel, we pad the edges so we can apply the convolutional kernel to the edges of the image without breaking; we take the max of the two padding values to ensure that we have enough padding for both filters
            ## padding is applied to ensure that the output has the same spatial dimensions as the input; if weights are None, this step is skipped and x remains unchanged
            if w is not None:
                x = torch.nn.functional.conv2d(x, w, padding=w_pad)
        ## bias is added to the output of the convolutional layer if bias is not None; the bias is reshaped to match the dimensions of x for broadcasting during addition
        if b is not None:
            x = x.add_(b.reshape(1, -1, 1, 1))
        return x

#----------------------------------------------------------------------------
# Group normalization.
## why we are doing this -- each layer the values are forced back to a reasonable range to avoid exploding or vanishing gradients
##  we do this by normalizing the values across groups of channels, which helps to stabilize training and improve convergence
### a channel is a small feature map, that abstractly represents a certain feature in the image
## group normalization is a variant of batch normalization that is more effective for small batch sizes, as it normalizes across groups of channels rather than across the entire batch
class GroupNorm(torch.nn.Module):
    def __init__(self, num_channels, num_groups=32, min_channels_per_group=4, eps=1e-5):
        super().__init__()
        # number of groups we want to split the channels into
        self.num_groups = min(num_groups, num_channels // min_channels_per_group)
        # eps - value added to denominator for numerical stability
        self.eps = eps
        # weight + bias used for rescaling after normalization; initialized to ones and zeros
        # 1 - normalize to mean 0 and variance 1; 2 - rescale and shift to original range per channel
        self.weight = torch.nn.Parameter(torch.ones(num_channels))
        self.bias = torch.nn.Parameter(torch.zeros(num_channels))

    def forward(self, x):
        # split channels in groups => calculate mean and std to normalize per group => rescale and shift with w and b per channel
        x = torch.nn.functional.group_norm(x, num_groups=self.num_groups, weight=self.weight.to(x.dtype), bias=self.bias.to(x.dtype), eps=self.eps)
        return x

#----------------------------------------------------------------------------
# Attention weight computation, i.e., softmax(Q^T * K).
# Performs all computation using FP32, but uses the original datatype for
# inputs/outputs/gradients to conserve memory.

class AttentionOp(torch.autograd.Function):
    @staticmethod
    def forward(ctx, q, k):
        # compute attention weights using the scaled dot-product attention formula; 
        # we use torch.einsum to compute the dot product between q and k, and then apply softmax to get the attention weights
        # result is stored in w, which is returned as the output of the forward pass; we also save q, k, and w for use in the backward pass
        w = torch.einsum('ncq,nck->nqk', q.to(torch.float32), (k / np.sqrt(k.shape[1])).to(torch.float32)).softmax(dim=2).to(q.dtype)
        ctx.save_for_backward(q, k, w)
        return w

    # backward pass with number formatting for memory efficiency
    @staticmethod
    def backward(ctx, dw):
        q, k, w = ctx.saved_tensors
        db = torch._softmax_backward_data(grad_output=dw.to(torch.float32), output=w.to(torch.float32), dim=2, input_dtype=torch.float32)
        dq = torch.einsum('nck,nqk->ncq', k.to(torch.float32), db).to(q.dtype) / np.sqrt(k.shape[1])
        dk = torch.einsum('ncq,nqk->nck', q.to(torch.float32), db).to(k.dtype) / np.sqrt(k.shape[1])
        # dq; dk - gradients wrt projection matrices q and k
        return dq, dk

#----------------------------------------------------------------------------
# Unified U-Net block with optional up/downsampling and self-attention.
# Represents the union of all features employed by the DDPM++, NCSN++, and
# ADM architectures.

# 1 UNet-Block - one processing step that takes a feature map, transforms it, and passes it on; one step in the overall UNet architecture
class UNetBlock(torch.nn.Module):
    def __init__(self,
        in_channels, out_channels, emb_channels, up=False, down=False, attention=False,
        num_heads=None, channels_per_head=64, dropout=0, skip_scale=1, eps=1e-5,
        resample_filter=[1,1], resample_proj=False, adaptive_scale=True,
        init=dict(), init_zero=dict(init_weight=0), init_attn=None,
    ):
        super().__init__()
        # number of channels in the input feature map; same as the previous blocks output channels; output channel = # kernels
        self.in_channels = in_channels
        # number of channels in the output feature map; conv0 transforms from in_channels to out_channels
        self.out_channels = out_channels
        # size of the vector that carries all the conditioning information into the U-Net Block (context for processing). they are one flat vector
        self.emb_channels = emb_channels
        # determines whether we use attention in the block to assess long-range dependencies in the feature map
        # attention is only applied when the feature map is already shrinked a lot by the convolutional layers (small map with many channels)
        self.num_heads = 0 if not attention else num_heads if num_heads is not None else out_channels // channels_per_head
        # fraction of activations is zeroed out during training to prevent overfitting
        self.dropout = dropout
        # prevent activations from growing too large as they accumulate across many blocks
        self.skip_scale = skip_scale
        # adaptive scale is a mechanism where the network learns to independently scale the normalized activations for each channel based on cond. info (noise level etc)
        self.adaptive_scale = adaptive_scale
        # normalize input before first convolution (uses in_channels because the input hasn't been transformed yet)
        self.norm0 = GroupNorm(num_channels=in_channels, eps=eps)
        # main convolutional layer; the channel count is trnasformed from in_channels to out_channels based on the kernel size; up/downsampling applied eventually
        # resampling filter is applied for up/downsampling to avoid artifacts
        self.conv0 = Conv2d(in_channels=in_channels, out_channels=out_channels, kernel=3, up=up, down=down, resample_filter=resample_filter, **init)
        # the embedding vector is transformed to a set of parameters where for each channel there is a scale and shift value (if adaptive scale is True)
        # no adaptive scale => only shift 
        self.affine = Linear(in_features=emb_channels, out_features=out_channels*(2 if adaptive_scale else 1), **init)
        # normalize output of first convolutional layer
        self.norm1 = GroupNorm(num_channels=out_channels, eps=eps)
        # conditioned features (so after affine trafo) are refined; the channel count is preserved
        self.conv1 = Conv2d(in_channels=out_channels, out_channels=out_channels, kernel=3, **init_zero)
        # by default the self.skip function does nothing
        self.skip = None
        # if the main feature map changed the shape (channel count difference or up/downsampling), then the original input is transformed to match the shape of the output
        # so after we transformed the feature map we add it to the original input (residual connection)
        if out_channels != in_channels or up or down:
            # when we want to resample projections or the number of channels changes we use as many 1x1 kernels as we have output channels to transform the input to the right shape
            ## when up/down and we have the same number of channels and no resample_proj we do bilinear interpolation
            ## when in != out we slide with out_channels 1x1 kernels over the picture with in_channels, so the in_channels get mixed together to produce out_channels
            kernel = 1 if resample_proj or out_channels!= in_channels else 0
            # the ctual convolutional layer that transforms the input to the right shape for the output so we can use the skip connection to add the original input to the output
            self.skip = Conv2d(in_channels=in_channels, out_channels=out_channels, kernel=kernel, up=up, down=down, resample_filter=resample_filter, **init)

        # this only runs if the UNet block has attention
        if self.num_heads:
            # group normalizatio before attention, stabilize activations before computation of attention weights
            self.norm2 = GroupNorm(num_channels=out_channels, eps=eps)
            # compute qkv projections for attention, we take 3 times the original output channel
            self.qkv = Conv2d(in_channels=out_channels, out_channels=out_channels*3, kernel=1, **(init_attn if init_attn is not None else init))
            # final projection layer that combines the different attention results
            self.proj = Conv2d(in_channels=out_channels, out_channels=out_channels, kernel=1, **init_zero)

    def forward(self, x, emb):
        # original input is saved for residual connection later on
        orig = x
        # normalzing input -> silu-activation function -> first convolutional layer
        x = self.conv0(silu(self.norm0(x)))
        # get affine parameters from the embedding (conditioning) vector
        # adaptive_scale is always True in this project, so affine outputs (batch, out_channels*2)
        # (scale+shift, split below) -> (batch, out_channels*2, 1, 1) so it broadcasts over all spatial positions
        params = self.affine(emb).unsqueeze(2).unsqueeze(3).to(x.dtype)

        # when we have shift and scale parameter
        if self.adaptive_scale:
            # we split the params object into two parts (scale & shift), then we apply it on the normalized feature map after the first convolutional layer
            scale, shift = params.chunk(chunks=2, dim=1)
            x = silu(torch.addcmul(shift, self.norm1(x), scale + 1))
        else:
            # no scaling, only shift
            x = silu(self.norm1(x.add_(params)))

        # during training we apply dropout; then we rund the second conv layer so the conditioned features are refined
        x = self.conv1(torch.nn.functional.dropout(x, p=self.dropout, training=self.training))
        # residual connection - when trafo is enabled orig gets transformed else just added on x
        x = x.add_(self.skip(orig) if self.skip is not None else orig)
        # output gets scaled down slightly to prevent activations from growing too large as they accumulate across many blocks
        x = x * self.skip_scale

        # when there are attention heads, attention is performed at the end
        if self.num_heads:
            # q, k, v get derived; first output of 2nd conv layer gets normalized
            ## (batch, out_channels*3, height, width) → (batch * num_heads, out_channels // num_heads, 3, height*width) after reshape
            ## then we unbind along the second dimension so we get three separate tensors
            q, k, v = self.qkv(self.norm2(x)).reshape(x.shape[0] * self.num_heads, x.shape[1] // self.num_heads, 3, -1).unbind(2)
            # computation of attention weights via softmax(Q*KT)
            w = AttentionOp.apply(q, k)
            # application of attention weights on value vectors
            a = torch.einsum('nqk,nck->ncq', w, v)
            # attention result is projected back to original channel count and then add to the output after the 2nd convolutional layer
            x = self.proj(a.reshape(*x.shape)).add_(x)
            # scale down to avoid exploding activations
            x = x * self.skip_scale
        return x

#----------------------------------------------------------------------------
# Timestep embedding used in the DDPM++ and ADM architectures.

class PositionalEmbedding(torch.nn.Module):
    def __init__(self, num_channels, max_positions=10000, endpoint=False):
        super().__init__()
        self.num_channels = num_channels
        self.max_positions = max_positions
        self.endpoint = endpoint

    def forward(self, x):
        freqs = torch.arange(start=0, end=self.num_channels//2, dtype=torch.float32, device=x.device)
        freqs = freqs / (self.num_channels // 2 - (1 if self.endpoint else 0))
        freqs = (1 / self.max_positions) ** freqs
        x = x.ger(freqs.to(x.dtype))
        x = torch.cat([x.cos(), x.sin()], dim=1)
        return x

#----------------------------------------------------------------------------
# Timestep embedding used in the NCSN++ architecture.

class FourierEmbedding(torch.nn.Module):
    def __init__(self, num_channels, scale=16):
        super().__init__()
        self.register_buffer('freqs', torch.randn(num_channels // 2) * scale)

    def forward(self, x):
        x = x.ger((2 * np.pi * self.freqs).to(x.dtype))
        x = torch.cat([x.cos(), x.sin()], dim=1)
        return x

#----------------------------------------------------------------------------
# Reimplementation of the ADM architecture from the paper
# "Diffusion Models Beat GANS on Image Synthesis". Equivalent to the
# original implementation by Dhariwal and Nichol, available at
# https://github.com/openai/guided-diffusion

class UNet(torch.nn.Module):
    def __init__(self,
        img_resolution,                     # Image resolution at input/output.
        in_channels,                        # Number of color channels at input.
        out_channels,                       # Number of color channels at output.
        label_dim           = 0,            # Number of class labels, 0 = unconditional.
        augment_dim         = 0,            # Augmentation label dimensionality, 0 = no augmentation.

        model_channels      = 128,          # Base multiplier for the number of
                 # channels.
        channel_mult        = [1,2,3,4],    # Per-resolution multipliers for the number of channels.
        channel_mult_emb    = 4,            # Multiplier for the dimensionality of the embedding vector.
        num_blocks          = 2,            # Number of residual blocks per resolution.
        attn_resolutions    = [32,16,8],    # List of resolutions with self-attention.
        dropout             = 0.10,         # List of resolutions with self-attention.
        label_dropout       = 0,            # Dropout probability of class labels for classifier-free guidance.
        use_diffuse = True                  # Use Unet for diffusion
    ):
        super().__init__()
        # probability labels (which are in cliamte down-scaling; day + hour) are randomly dropped during training to make model robust to missing labels during inference
        self.label_dropout = label_dropout
        # how many channels we have in the model; base level is model_channels (64 in this project), embedding is model_channels * channel_mult_emb (256 here)
        emb_channels = model_channels * channel_mult_emb
        # initialization configuration for the layers; 
        init = dict(init_mode='kaiming_uniform', init_weight=np.sqrt(1/3), init_bias=np.sqrt(1/3))
        # initializatin confiugration for layers that do nothing at the beginning of training
        init_zero = dict(init_mode='kaiming_uniform', init_weight=0, init_bias=0)
        # dictionary for the previous arguments so we can call with **kwargs
        block_kwargs = dict(emb_channels=emb_channels, channels_per_head=64, dropout=dropout, init=init, init_zero=init_zero)

        # Mapping.
        # noise level is encoded to a vector of size model_channels (64 in this project, not the class default of 128); like pos encoding in transformer
        self.map_noise = PositionalEmbedding(num_channels=model_channels) if use_diffuse else None
        # project augmentation labels;; not done in this context
        self.map_augment = Linear(in_features=augment_dim, out_features=model_channels, bias=False, **init_zero) if augment_dim else None
        # project encoding of noise level from 128 dim vector to 512 dim vector
        self.map_layer0 = Linear(in_features=model_channels, out_features=emb_channels, **init)
        # second linear layer; together with map_layer0 forms a 2-layer MLP for nonlinear noise encoding
        self.map_layer1 = Linear(in_features=emb_channels, out_features=emb_channels, **init)
        # project class label (day, hour) to embedding vector 
        self.map_label = Linear(in_features=label_dim, out_features=emb_channels, bias=False, init_mode='kaiming_normal', init_weight=np.sqrt(label_dim)) if label_dim else None

        # sanity check - image resolution must have length 2 (height x width)
        assert len(img_resolution) == 2

        # Encoder. - downsampling half of the UNet
        ## build dictionary that scores all encoder layers by name
        self.enc = torch.nn.ModuleDict()
        # number of input_channels to the UNet; one kernel operates across all input channels simulateneously; 3x3 kernel collapses the 3x3xin-channels cube of values to a single number
        cout = in_channels
        # level -- depth index (how much the spatial size has been halved by downsampling)
        # mult -- channel multiplier at that level (1,2,3,4)
        for level, mult in enumerate(channel_mult):
            # compute spatial resolution for x and y at each level (mainly for dict only)
            resx = img_resolution[0] >> level
            resy = img_resolution[1] >> level
            # first level/full resolution
            if level == 0:
                # number of input channels to the first convolutional layer
                cin = cout
                # number of output channels of the first convolutional layer
                cout = model_channels * mult
                # first convolutional layer gets transformed so that we get from the raw input channels to the base number of feature channels (no downsampling)!
                # just project the inputs into the feature space
                self.enc[f'{resx}x{resy}_conv'] = Conv2d(in_channels=cin, out_channels=cout, kernel=3, **init)
            else:
                # now we are in level 1,2,3 etc where we are already in the feature space and we just can use the UNet Block to downsample
                # here we are only defining the downsampling part for the level of the Encoder
                ## !! number of channels is not changed here
                ## !! down = True; so the spatial resolution is changed here
                self.enc[f'{resx}x{resy}_down'] = UNetBlock(in_channels=cout, out_channels=cout, down=True, **block_kwargs)
            # num_blocks at each resolution level; for every layer there are 2 processing blocks are the initial convolution/downsampling block
            for idx in range(num_blocks):
                # save the number of output channels of the prev block as the nmber of input channels for this block
                cin = cout
                # how many channels we should have after this block, channel count is changed because cout is set anew
                cout = model_channels * mult
                # now we define the next encoder block for this level, now we use a UNet block that changes the number of channels and does attention afterwards
                ## !! spatial resolution stays the same here, only channel count is changed the first time we go through that block, the 2nd time it is not changed
                self.enc[f'{resx}x{resy}_block{idx}'] = UNetBlock(
                    in_channels=cin, out_channels=cout, attention=(resx in
                                                                   attn_resolutions), **block_kwargs)
        # collect the output channel count of every encoder layer into a list, in order
        skips = [block.out_channels for block in self.enc.values()]

        # Decoder.
        # track the module as decoder
        self.dec = torch.nn.ModuleDict()
        #  we go through the levels in reverse order
        ## ((3,4), (2,3), (1,2), (0,1)) -- decoder goes from deepest to shallowest level
        for level, mult in reversed(list(enumerate(channel_mult))):
            ## compute spatial resolution for x and y at each level (mainly for dict only)
            resx = img_resolution[0] >> level
            resy = img_resolution[1] >> level
            ## at the deepest level
            if level == len(channel_mult) - 1:
                # in the deepest layer there is no upsampling
                ## the first UNet block processes the deepest features with attention
                self.dec[f'{resx}x{resy}_in0'] = UNetBlock(in_channels=cout,
                                                          out_channels=cout, attention=True, **block_kwargs)
                ## then we do a 2nd round of processing, now without attention
                self.dec[f'{resx}x{resy}_in1'] = UNetBlock(in_channels=cout,
                                                          out_channels=cout, **block_kwargs)
            else:
                ## when we are not at the deepest level, we do upsample; channel count is never changed
                self.dec[f'{resx}x{resy}_up'] = UNetBlock(in_channels=cout,
                                                         out_channels=cout, up=True, **block_kwargs)
            ## now we iterate through the 3 blocks at the decoder level after the initial block (2 for deepest layer, else 1)
            for idx in range(num_blocks + 1):
                # the encoder skip connection from before is concatenated to the decoder block after its initial blocks
                # channel number gets doubled here!!
                # so we expect double as many cin for this block
                cin = cout + skips.pop()
                # the number of output chanels is iteratively decreased
                cout = model_channels * mult
                ## decoder UNet Block is created and stored in the Modul.Dict
                ## !! for the first block the number of channels decreases, then the channels number stays the same, then the target cout for this level is acheived already
                ## attention is applied when the resolution is really low
                self.dec[f'{resx}x{resy}_block{idx}'] = UNetBlock(
                    in_channels=cin, out_channels=cout, attention=(resx in
                                                                   attn_resolutions), **block_kwargs)
        # final normalization on the last decoder output to stabilize activations before final convolution
        self.out_norm = GroupNorm(num_channels=cout)
        # now the final convolution layer transforms back to the desired number of output channels
        self.out_conv = Conv2d(in_channels=cout, out_channels=out_channels, kernel=3, **init_zero)

    def forward(self, x, noise_labels=None, class_labels=None,
                augment_labels=None):
        # Mapping  --- CONDITIONAL VECTOR CREATION
        ## Initialize the embedding vector to zero; default behavior when there is no label cnditioning
        ## shape [1, emb_channels] -- emb_channels = model_channels * channel_mult_emb = 256 in this project (not the class default of 512)
        emb = torch.zeros([1, self.map_layer1.in_features], device=x.device)
        ## this runs only when we provide the labels
        if self.map_label is not None:
            ## labels copied to placeholder tmp
            tmp = class_labels
            ## labels are dropped during training
            if self.training and self.label_dropout:
                ## randomly set some labels to zero with probability label_dropout; this is done to make the model robust to missing labels during inference
                tmp = tmp * (torch.rand([x.shape[0], 1],
                                        device=x.device) >= self.label_dropout).to(
                    tmp.dtype)
            # label gets projected to a 512-dim vector
            emb = self.map_label(tmp)
        # always true because we always do diffusion here
        if self.map_noise is not None:
            # encode noise-level as scalar to a model_channels-dim vector (64 here)
            emb_n = self.map_noise(noise_labels)
            # expand noise level encoding from model_channels to emb_channels (64 to 256 here) and then do silu activation
            emb_n = silu(self.map_layer0(emb_n))
            # further transforms noise level using 2nd layer
            emb_n = self.map_layer1(emb_n)
            # add noise embedding to label embedding (so now one 512-dim conditioning vector)
            emb = emb + emb_n

        ## NOT USED HERE BECAUSE NO AUGMENTATION
        if self.map_augment is not None and augment_labels is not None:
            emb = emb + self.map_augment(augment_labels)

        # Final non-linear activation on the complete embedding vector (so finalizing condtional embeding vector)
        emb = silu(emb)

        # Encoder.
        ## empty list to store encoder outputs for skip connections
        skips = []
        # iterate over all encoder layers in order (conv, down blocks, processing blocks)
        for block in self.enc.values():
            # if we are at the very start where we project our data into the feature space, we do not use the embedding vector, at the start simpele CNN
            # afterwards when we process using UNets we also pass the embeding vector
            x = block(x, emb) if isinstance(block, UNetBlock) else block(x)
            ## saves the output of every layer, used for the skip connections in the decoder later on
            skips.append(x)

        # Decoder.
        # iterate over all decoder layers in order (bottleneck blocks; upsampling blocks; processing blocks)
        for block in self.dec.values():
            ## when the shape of our object is not equal to the shape we expect from the block we concatenate the skip connection from the encoer to x
            if x.shape[1] != block.in_channels:
                x = torch.cat([x, skips.pop()], dim=1)
            ## we run the decoder block with the conditoning embedding vector for as many blocks as we have defined
            x = block(x, emb)
        # final normalization, activation function (silu) and convolution to ge tback to the desired number of output channels
        x = self.out_conv(silu(self.out_norm(x)))
        return x

#----------------------------------------------------------------------------
# Improved preconditioning proposed in the paper "Elucidating the Design
# Space of Diffusion-Based Generative Models" (EDM).

## wrapper around Unet that applies improved preconditioning proposed by EDM paper, scaling of inputs, outputs, and noise levels

class EDMPrecond(torch.nn.Module):
    def __init__(self,
        img_resolution,                     # Image resolution.
        in_channels,                       # Number of color channels.
        out_channels,                       # Number of color channels.
        label_dim       = 0,                # Number of class labels, 0 = unconditional.
        use_fp16        = False,            # Execute the underlying model at FP16 precision?
        sigma_min       = 0,                # Minimum supported noise level.
        sigma_max       = float('inf'),     # Maximum supported noise level.
        sigma_data      = 1.0,              # Expected standard deviation of
                 # the training data.
        model_type      = 'UNet',   # Class name of the underlying model.
        **model_kwargs,                     # Keyword arguments for the underlying model.
    ):
        super().__init__()
        self.img_resolution = img_resolution
        self.in_channels = in_channels
        self.out_channels = out_channels
        self.label_dim = label_dim
        self.use_fp16 = use_fp16
        self.sigma_min = sigma_min
        self.sigma_max = sigma_max
        self.sigma_data = sigma_data
        self.model = globals()[model_type](
            img_resolution=img_resolution, in_channels=in_channels,
            out_channels=out_channels, label_dim=label_dim, **model_kwargs)

    def forward(self, x, sigma, condition_img=None, class_labels=None,
                force_fp32=True, **model_kwargs):
        if condition_img is not None:
            in_img = torch.cat([x, condition_img], dim=1)
        else:
            in_img = x
        sigma = sigma.reshape(-1, 1, 1, 1)
        class_labels = None if self.label_dim == 0 else torch.zeros([1, self.label_dim], device=in_img.device) if class_labels is None else class_labels.to(torch.float32).reshape(-1, self.label_dim)
        dtype = torch.float16 if (self.use_fp16 and not force_fp32 and in_img.device.type == 'cuda') else torch.float32

        c_skip = self.sigma_data ** 2 / (sigma ** 2 + self.sigma_data ** 2)
        c_out = sigma * self.sigma_data / (sigma ** 2 + self.sigma_data ** 2).sqrt()
        c_in = 1 / (self.sigma_data ** 2 + sigma ** 2).sqrt()
        c_noise = sigma.log() / 4

        F_x = self.model((c_in * in_img).to(dtype),
                         noise_labels=c_noise.flatten(),
                         class_labels=class_labels, **model_kwargs).to(dtype)
        assert F_x.dtype == dtype
        D_x = c_skip * x + c_out * F_x
        return D_x

    def round_sigma(self, sigma):
        return torch.as_tensor(sigma)

#----------------------------------------------------------------------------

