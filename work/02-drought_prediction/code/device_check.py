"""Device-consistency smoke test for the unrolled ConvLSTM.

Builds a model with tiny dimensions, registers fake buffers, runs one forward
+ backward step through a synthetic batch, and asserts that every parameter and
every registered buffer lives on the same device as the input batch.

Run from the project root:
    python code/device_check.py
    python code/device_check.py --accelerator cpu
    python code/device_check.py --accelerator cuda     # on the cluster
"""

import argparse
import logging
import sys
from pathlib import Path

# triton is Linux/CUDA-compiler-only; this warning is a no-op on our setup.
# Must be set before `import torch` — flop_counter logs it at import time.
logging.getLogger("torch.utils.flop_counter").setLevel(logging.ERROR)

import torch  # noqa: E402
from pytorch_lightning import Trainer, seed_everything  # noqa: E402

sys.path.insert(0, str(Path(__file__).parent))
sys.stdout.reconfigure(encoding="utf-8")
sys.stderr.reconfigure(encoding="utf-8")

from model import RCNNModule  # noqa: E402

# Tiny dims — just enough to run without the real dataset
H, W   = 8, 8
T      = 4      # history length
B      = 2
N_VARS = 5      # SPEI + 4 dynamic


def _make_model(H: int, W: int, T: int, N_VARS: int) -> RCNNModule:
    model = RCNNModule(
        mode="regression",
        embedding_size=4,
        hidden_state_size=8,
        kernel_size=3,
        n_cells_hor=H,
        n_cells_ver=W,
        history_length=T,
        periods_forward=1,
        batch_size=B,
        num_of_additional_features=N_VARS - 1,
        loss_fn="mse",
        static_encoder="none",
        global_encoder="none",
        n_extra_channels=0,
        dropout=0.0,
        lr=1e-3,
        weight_decay=0.0,
        lr_scheduler_patience=5,
    )
    # Register the buffers that train.py normally provides
    model.register_buffer("mask",       torch.ones(H, W, dtype=torch.bool))
    model.register_buffer("global_avg", torch.zeros(H, W))
    model.register_buffer("trend_test", torch.zeros(10, H, W))
    model.register_buffer("rlat",       torch.zeros(H))
    model.register_buffer("rlon",       torch.zeros(W))
    return model


def _check_device_consistency(model: RCNNModule, batch_device: torch.device) -> list[str]:
    """Return list of mismatches: (name, expected, actual)."""
    mismatches = []
    for name, param in model.named_parameters():
        if param.device != batch_device:
            mismatches.append(f"  param  {name}: expected {batch_device}, got {param.device}")
    for name, buf in model.named_buffers():
        if buf is None:
            continue
        if buf.device != batch_device:
            mismatches.append(f"  buffer {name}: expected {batch_device}, got {buf.device}")
    return mismatches


def main():
    parser = argparse.ArgumentParser(description=__doc__,
                                     formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--accelerator", default="auto",
                        help="Accelerator for Trainer (auto / cpu / cuda / mps)")
    parser.add_argument("--precision", default="32-true",
                        help='Trainer precision, e.g. "32-true", "bf16-mixed"')
    parser.add_argument("--deterministic", default="warn",
                        help='Trainer deterministic setting: true / false / warn')
    args = parser.parse_args()

    # Parse deterministic flag (accepts bool-like strings or "warn")
    det = args.deterministic
    if det.lower() == "true":
        det = True
    elif det.lower() == "false":
        det = False
    # else leave as string "warn"

    seed_everything(42)

    print("=" * 60)
    print("DEVICE CHECK — unrolled ConvLSTM")
    print(f"  accelerator={args.accelerator!r}  precision={args.precision!r}"
          f"  deterministic={det!r}")
    print("=" * 60)

    model = _make_model(H, W, T, N_VARS)

    # One-step Trainer to force Lightning to move model to the resolved device
    trainer = Trainer(
        max_epochs=1,
        limit_train_batches=2,
        limit_val_batches=1,
        logger=False,
        enable_checkpointing=False,
        enable_progress_bar=False,
        accelerator=args.accelerator,
        devices=1,
        precision=args.precision,
        deterministic=det,
    )

    # Synthetic dataset: (B, T, N_VARS, H, W) inputs + (B, 1, H, W) targets
    class _TinyDS(torch.utils.data.Dataset):
        def __len__(self):
            return 8
        def __getitem__(self, _):
            return torch.randn(T, N_VARS, H, W), torch.randn(1, H, W)

    loader = torch.utils.data.DataLoader(_TinyDS(), batch_size=B)
    trainer.fit(model, loader, loader)

    # After fit, check every parameter and buffer is on the resolved device
    resolved = next(model.parameters()).device
    print(f"\nResolved device after Trainer.fit: {resolved}")

    mismatches = _check_device_consistency(model, resolved)
    if mismatches:
        print("\nFAIL — device mismatches:")
        for m in mismatches:
            print(m)
        sys.exit(1)
    else:
        print("PASS — all parameters and buffers are on the same device.")

    # Confirm a manual forward pass produces a finite loss
    model.eval()
    x = torch.randn(B, T, N_VARS, H, W, device=resolved)
    y = torch.randn(B, 1, H, W, device=resolved)
    with torch.no_grad():
        pred = model(x)
    loss = ((pred - y) ** 2).mean()
    finite = loss.isfinite().item()
    print(f"Forward pass: pred shape={tuple(pred.shape)}  loss={loss.item():.6f}  finite={finite}")
    if not finite:
        print("FAIL — loss is not finite")
        sys.exit(1)

    print("\nDEVICE CHECK COMPLETE — all assertions passed.")
    print("=" * 60)


if __name__ == "__main__":
    main()
