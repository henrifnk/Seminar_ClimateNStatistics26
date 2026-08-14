"""
Training loop for the EDM-preconditioned diffusion downscaler.

Trains on CERRA coarse/fine residual pairs (see DatasetAL.py) with the EDM loss, mixed
precision, gradient accumulation and an EMA copy of the weights. Splits are
train 2000-2016, validation 2017-2018, test 2019-2020.

Per-epoch sample grids go to plots/training/; losses and figures are logged to Weights &
Biases. Normalization statistics are written to data/norm_stats.npz as a side effect â€”
ComputeNormStats.py regenerates them standalone.

Note that the validation denoising loss rewards conditional-mean collapse, so it is not a
valid stopping criterion for generative quality; compare checkpoints on ensemble-level
diagnostics instead.

Usage:
    python src/training/TrainDiffusion.py
"""
import copy
import os
import sys
from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np
import torch
from tqdm import tqdm

import wandb

_SRC_DIR = Path(__file__).resolve().parent.parent
if str(_SRC_DIR) not in sys.path:
    sys.path.insert(0, str(_SRC_DIR))

import wm2024model
from DatasetAL import UpscaleDataset, save_norm_stats
from paths import TRAINING_DIR


class EDMLoss:
    def __init__(self, P_mean=-1.2, P_std=1.2, sigma_data=1.0):
        # mean of log-normal distribution from which sigma is sampled
        self.P_mean = P_mean
        # std of log-normal distribution from which sigma is sampled
        self.P_std = P_std
        # expected std of the training data
        self.sigma_data = sigma_data

    def __call__(self, net, images, conditional_img=None, labels=None,
                 augment_pipe=None):
        # we ge tone scalar (noise level) per image in the batch, sampled from a log-normal distribution
        rnd_normal = torch.randn([images.shape[0], 1, 1, 1], device=images.device)
        # compute the noise level sigma for each image in the batch
        sigma = (rnd_normal * self.P_std + self.P_mean).exp()
        # compute the weight for the loss function based on the noise level and expected data std
        # evening out the loss across different noise levels; higher noise levels get lower weight, lower noise levels get higher weight
        weight = (sigma ** 2 + self.sigma_data ** 2) / (sigma * self.sigma_data) ** 2
        # augmentation shenanigans
        y, augment_labels = augment_pipe(images) if augment_pipe is not None else (images, None)
        mask = ~torch.isnan(y)
        # replace NaNs with zeros to avoid NaN loss values; sea pixels are NaN, so we mask them out
        y = torch.nan_to_num(y, nan=0.0)
        # create the noise the network has to learn to remove
        n = torch.randn_like(y) * sigma
        # add the noise on the image to get the corrupted image, then apply the mask to ignore NaN pixels
        noisy = (y + n) * mask
        # corrupted image (y + n) along with noise level sigma is passed to the network ; network knows how noisy input is; coarse image + orography, and time labels
        D_yn = net(noisy, sigma, conditional_img, labels, augment_labels=augment_labels)
        # D_yn is the network's prediction of the clean image y (EDMPrecond returns denoised output, not noise)
        # weighted MSE between predicted clean image and true clean image, only over valid land pixels
        # weight applied because the loss will be higher when we have a higher noise level (predicted image will be off way more)
        loss = weight * ((D_yn - y) ** 2) * mask
        return loss


DEVICE = 'cuda' if torch.cuda.is_available() else 'cpu'


def training_step(model, loss_fn, optimiser, data_loader, scaler, step,
                  ema_model, ema_decay, accum=4, device=DEVICE,
                  cond_dropout_p=0.1):
    # put the model in training moden (enables dropout, batchnorm, etc.)
    model.train()
    # wrap the loop to show progress bar
    with tqdm(total=len(data_loader), dynamic_ncols=True) as tq:
        tq.set_description(f"Train :: Epoch: {step}")
        epoch_losses = []
        step_loss = 0
        for i, batch in enumerate(data_loader):
            tq.update(1)
            image_input = batch["inputs"].to(device)
            image_output = batch["targets"].to(device)
            condition_params = torch.stack(
                (batch["doy"].to(device), batch["hour"].to(device)), dim=1)

            # Zero out conditioning for a random subset of samples in the batch, so we remove the contextual information for these samples
            if cond_dropout_p > 0:
                drop = (torch.rand(image_input.shape[0], 1, 1, 1, device=device)
                        < cond_dropout_p)
                image_input = image_input * (~drop)

            with torch.amp.autocast('cuda'):
                # loss is one loss value per pixel, per channel, per sample in th ebatch
                loss = loss_fn(net=model, images=image_output,
                               conditional_img=image_input,
                               labels=condition_params)
                # average the loss over all pixels, channels, and samples in the batch to get a single scalar loss value
                loss = torch.mean(loss)
            # scale loss up and then calculate the gradients
            scaler.scale(loss).backward()
            step_loss += loss.item()
            # gradient accumulation + optimizer step
            if (i + 1) % accum == 0:
                scaler.step(optimiser)
                scaler.update()
                optimiser.zero_grad(set_to_none=True)
                # EMA update for model parameters per optimizer step
                with torch.no_grad():
                    for ema_p, p in zip(ema_model.parameters(), model.parameters()):
                        ema_p.data.mul_(ema_decay).add_(p.data, alpha=1 - ema_decay)
                # epoch loss tracking
                wandb.log({"Loss/train": step_loss / accum},
                          step=step * len(data_loader) + i)
                step_loss = 0

            epoch_losses.append(loss.item())
            tq.set_postfix_str(s=f"Loss: {loss.item():.4f}")
        # mean loss per epoch is returned
        mean_loss = sum(epoch_losses) / len(epoch_losses)
        tq.set_postfix_str(s=f"Loss: {mean_loss:.4f}")
    return mean_loss

@torch.no_grad()
def evaluate_model(model, loss_fn, dataloader, step, device=DEVICE):
    # set model in evaluation mode
    model.eval()
    epoch_losses = []
    # calculate loss and track it for each batch in the validation set, no gradient tracking needed
    with tqdm(total=len(dataloader), dynamic_ncols=True) as tq:
        tq.set_description(f"Val :: Epoch: {step}")
        for i, batch in enumerate(dataloader):
            tq.update(1)
            image_input = batch["inputs"].to(device)
            image_output = batch["targets"].to(device)
            day = batch["doy"].to(device)
            hour = batch["hour"].to(device)
            condition_params = torch.stack((day, hour), dim=1)

            with torch.amp.autocast('cuda'):
                loss = loss_fn(net=model, images=image_output,
                               conditional_img=image_input,
                               labels=condition_params)
                loss = torch.mean(loss)

            epoch_losses.append(loss.item())
            tq.set_postfix_str(s=f"Loss: {loss.item():.4f}")
        mean_loss = sum(epoch_losses) / len(epoch_losses)
        tq.set_postfix_str(s=f"Loss: {mean_loss:.4f}")
    return mean_loss

def _rms_laplacian(x: torch.Tensor) -> float:
    """RMS of Laplacian response over all valid (non-NaN) pixels. Higher = sharper."""
    x = torch.nan_to_num(x.float(), nan=0.0)
    if x.dim() == 3:
        x = x.unsqueeze(0)
    B, C, H, W = x.shape
    kernel = torch.tensor([[0., 1., 0.], [1., -4., 1.], [0., 1., 0.]]).view(1, 1, 3, 3)
    lap = torch.nn.functional.conv2d(x.reshape(B * C, 1, H, W), kernel, padding=1)
    return lap.pow(2).mean().sqrt().item()


def _nanstd(x: torch.Tensor) -> float:
    vals = x[~torch.isnan(x)]
    return vals.std().item() if vals.numel() > 1 else 0.0


# disable gradient tracking // like with torch-no_grad()
@torch.no_grad()
def generate_sample(model, dataloader, num_steps=40, sigma_min=0.002,
                    sigma_max=80, rho=7, S_churn=40, S_min=0,
                    S_max=float('inf'), S_noise=1, device=DEVICE):
    """Run the diffusion sampler on one batch and return raw prediction tensors.

    Returns a dict with keys: predicted, coarse, fine, coarse_raw
    Pass this dict to evaluate_sample() to get plots and error metrics.
    """
    # grab a batch from the test dataloader; we will run the sampling process on this batch and visualize the results
    batch = next(iter(dataloader))
    images_input = batch["inputs"].to(device)
    coarse, fine, coarse_raw = batch["coarse"], batch["fine"], batch["coarse_raw"]
    condition_params = torch.stack(
        (batch["doy"].to(device), batch["hour"].to(device)), dim=1)
    # clip noise level so that it is in the range in which the model was trained on
    sigma_min = max(sigma_min, model.sigma_min)
    sigma_max = min(sigma_max, model.sigma_max)
    # starting point of reverse diffusion process is pure noise
    # we generate a noise image with the same spatial dimensions as the input images
    init_noise = torch.randn((images_input.shape[0], 2, images_input.shape[2],
                              images_input.shape[3]),
                             dtype=torch.float64, device=device)
    # do as many indices as steps are available
    step_indices = torch.arange(num_steps, dtype=torch.float64, device=device)
    # compute the noise level for all steps (more steps with a medium noise level)
    t_steps = (sigma_max ** (1 / rho) + step_indices / (num_steps - 1)
               * (sigma_min ** (1 / rho) - sigma_max ** (1 / rho))) ** rho
    # round the noise levels to known noise levels for the model and add the noise level of 0 at the end
    t_steps = torch.cat([model.round_sigma(t_steps), torch.zeros_like(t_steps[:1])])
    # determine starting point of reverse diffusion process
    x_next = init_noise.to(torch.float64) * t_steps[0]

    # zip() pairs consecutive sigmas: (Ïƒ_0,Ïƒ_1), (Ïƒ_1,Ïƒ_2), ..., (Ïƒ_min, 0)
    for i, (t_cur, t_next) in enumerate(zip(t_steps[:-1], t_steps[1:])):
        # carries over result from previous step to the next step
        x_cur = x_next

        # stochastic churn: inject a small amount of noise before denoising to improve sample diversity
        # gamma is how much noise is injected if the current noise level is within the range of S_min and S_max
        gamma = min(S_churn / num_steps, np.sqrt(2) - 1) if S_min <= t_cur <= S_max else 0
        # snaps noise level to the nearest known noise level for the model and bumps sigma up by gamma
        t_hat = model.round_sigma(t_cur + gamma * t_cur)
        # increase variance of x from t_curÂ² to t_hatÂ² by adding the exact missing noise
        x_hat = x_cur + (t_hat ** 2 - t_cur ** 2).sqrt() * S_noise * torch.randn_like(x_cur)

        # --- Euler step ---
        # runs noisy image through the model to get the denoised image; the model knows how noisy the input is and can denoise it accordingly
        denoised = model(x_hat, t_hat, images_input, condition_params).to(torch.float64)
        # score (direction from current noisy image to denoised image) normalized by sigma
        d_cur = (x_hat - denoised) / t_hat
        # make a first-order step in the direction of the score, scaled by the sigma drop
        x_next = x_hat + (t_next - t_hat) * d_cur

        # --- Heun correction (2nd order) ---
        # re-evaluate score at the Euler-predicted position and average with d_cur for a more accurate step
        # we compute the denoised image at the next step and use it to correct our estimate of the next noisy image
        if i < num_steps - 1:  # skip on last step (t_next=0 would cause division by zero)
            denoised = model(x_next, t_next, images_input, condition_params).to(torch.float64)
            d_prime = (x_next - denoised) / t_next
            x_next = x_hat + (t_next - t_hat) * (0.5 * d_cur + 0.5 * d_prime)  # corrected step using average of both scores

    # convert the final denoised residual back to a physical climate field
    predicted = dataloader.dataset.residual_to_fine_image(x_next.detach().cpu(), coarse)
    return {"predicted": predicted, "coarse": coarse, "fine": fine, "coarse_raw": coarse_raw}


def evaluate_sample(sample, dataset):
    """Compute error metrics and plot for a sample dict returned by generate_sample().

    Returns (fig, ax), (base_error, pred_error)
    """
    predicted, coarse, fine, coarse_raw = (
        sample["predicted"], sample["coarse"], sample["fine"], sample["coarse_raw"])
    # create side-by-side plots of the coarse input, fine target, and predicted output for visual comparison
    fig, ax = dataset.plot_batch(coarse_raw, coarse, fine, predicted)
    plt.subplots_adjust(wspace=0, hspace=0)
    # MAE of bilinear baseline vs. ground truth (the floor the model needs to beat)
    base_error = torch.mean(torch.abs(fine - coarse))
    # MAE of model prediction vs. ground truth
    pred_error = torch.mean(torch.abs(fine - predicted))

    # Sharpness ratio: Laplacian RMS of pred / target. 1.0 = as sharp as ground truth.
    # Drops well below 1 when the model collapses toward conditional mean (blurring).
    sharp_ratio = _rms_laplacian(predicted) / (_rms_laplacian(fine) + 1e-8)

    # Residual std ratio: how much variance the model adds relative to ground truth residual.
    # Approaches 0 when the model predicts near-zero (smooth) residuals.
    res_pred_std = _nanstd(predicted - coarse)
    res_true_std = _nanstd(fine - coarse)
    res_std_ratio = res_pred_std / (res_true_std + 1e-8)

    return (fig, ax), (base_error.item(), pred_error.item(), sharp_ratio, res_std_ratio)


def sample_model(model, dataloader, **kwargs):
    """Convenience wrapper: generate a sample and evaluate it in one call."""
    sample = generate_sample(model, dataloader, **kwargs)
    return evaluate_sample(sample, dataloader.dataset)


def main():
    batch_size = 4
    learning_rate = 1e-4
    num_epochs = 20
    accum = 8
    use_ema = False
    ema_decay = 0.999
    cond_dropout_p = 0.0   # fraction of samples with conditioning zeroed out

    device = 'cuda' if torch.cuda.is_available() else 'cpu'

    # Model: 256x320 output, model_channels=64, in_channels=4 (noisy+coarse+lsm+orog), out=1
    network = wm2024model.EDMPrecond((256, 320), 6, 2, label_dim=2, model_channels=64)
    network.to(device)

    ema_network = copy.deepcopy(network) if use_ema else network

    datadir_fine   = "data/reanalysis/"
    datadir_coarse = "data/reanalysis_coarsened/"
    datadir_cons   = "data/"

    dataset_train = UpscaleDataset(datadir_fine, datadir_coarse, datadir_cons,
                                   year_start=2006, year_end=2007,
                                   constant_variables=["lsm", "orog"])
    dataset_val   = UpscaleDataset(datadir_fine, datadir_coarse, datadir_cons,
                                   year_start=2007, year_end=2008,
                                   constant_variables=["lsm", "orog"],
                                   normalize_rawdata_mean=dataset_train._norm_raw_mean,
                                   normalize_rawdata_std=dataset_train._norm_raw_std,
                                   normalize_residual_mean=dataset_train._norm_res_mean,
                                   normalize_residual_std=dataset_train._norm_res_std)
    dataset_test  = UpscaleDataset(datadir_fine, datadir_coarse, datadir_cons,
                                   year_start=2007, year_end=2008,
                                   constant_variables=["lsm", "orog"],
                                   normalize_rawdata_mean=dataset_train._norm_raw_mean,
                                   normalize_rawdata_std=dataset_train._norm_raw_std,
                                   normalize_residual_mean=dataset_train._norm_res_mean,
                                   normalize_residual_std=dataset_train._norm_res_std)

    save_norm_stats(dataset_train)

    dataloader_train = torch.utils.data.DataLoader(
        dataset_train, batch_size=batch_size, shuffle=True, num_workers=2, persistent_workers=True)
    dataloader_val   = torch.utils.data.DataLoader(
        dataset_val,   batch_size=batch_size, shuffle=True, num_workers=2, persistent_workers=True)
    dataloader_test  = torch.utils.data.DataLoader(
        dataset_test,  batch_size=batch_size, shuffle=True, num_workers=2, persistent_workers=True)

    scaler    = torch.amp.GradScaler('cuda')
    optimiser = torch.optim.AdamW(network.parameters(), lr=learning_rate, weight_decay=0.01)
    scheduler = torch.optim.lr_scheduler.ReduceLROnPlateau(
        optimiser, mode="min", factor=0.5, patience=3, threshold=1e-3, min_lr=1e-6)
    loss_fn   = EDMLoss()

    wandb.init(
        project="climate-diffusion-downscaling",
        config=dict(batch_size=batch_size, learning_rate=learning_rate,
                    num_epochs=num_epochs, accum=accum, use_ema=use_ema,
                    ema_decay=ema_decay if use_ema else None,
                    cond_dropout_p=cond_dropout_p)
    )

    os.makedirs("./model",   exist_ok=True)
    os.makedirs(TRAINING_DIR, exist_ok=True)

    for step in range(num_epochs):
        train_epoch_loss = training_step(
            network, loss_fn, optimiser, dataloader_train,
            scaler, step, ema_network, ema_decay if use_ema else None, accum,
            cond_dropout_p=cond_dropout_p)

        val_epoch_loss = evaluate_model(ema_network, loss_fn, dataloader_val, step)

        scheduler.step(val_epoch_loss)
        current_lr = optimiser.param_groups[0]["lr"]

        global_step = (step + 1) * len(dataloader_train)
        print(f"Epoch {step}: train={train_epoch_loss:.4f}  val={val_epoch_loss:.4f}  lr={current_lr:.2e}")
        log_dict = {"Loss/val": val_epoch_loss, "Loss/train_epoch": train_epoch_loss,
                    "epoch": step, "lr": current_lr}

        torch.manual_seed(42)
        (fig, ax), (base_error, pred_error, sharp_ratio, res_std_ratio) = sample_model(ema_network, dataloader_test)
        fig.savefig(f"{TRAINING_DIR}/{step}.png", dpi=150)
        log_dict.update({"Error/base": base_error, "Error/pred": pred_error,
                         "Metrics/sharpness_ratio": sharp_ratio,
                         "Metrics/residual_std_ratio": res_std_ratio,
                         "sample": wandb.Image(f"{TRAINING_DIR}/{step}.png")})
        print(f"  sharpness_ratio={sharp_ratio:.3f}  residual_std_ratio={res_std_ratio:.3f}")
        plt.close(fig)

        wandb.log(log_dict, step=global_step)

        torch.save(ema_network.state_dict(), f"./model/{step}.pt")


if __name__ == "__main__":
    main()

