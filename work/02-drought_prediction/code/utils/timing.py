"""Per-epoch wall-clock timer callback with device sync and warmup separation.

Designed to be reused by train.py and the device×batch benchmark:
  - device sync before every timer read (flushes async MPS/CUDA kernels)
  - epoch 0 tagged as warmup and logged separately (never mixed into steady-state mean)
  - compute-only time (sum of batch forward+backward+optimizer) vs end-to-end epoch wall
  - prints a one-line summary per epoch so throttling/drift is visible mid-run
"""

import time

import torch
from pytorch_lightning import Callback
from pytorch_lightning.trainer.trainer import Trainer


def device_sync(trainer: Trainer) -> None:
    """Flush pending async ops before reading a wall-clock timer.

    Must be called before every timer stop — without this, MPS/CUDA ops
    still in-flight at the time of the clock read make the measurement
    meaningless (the CPU races ahead of the GPU).
    """
    device_type = trainer.strategy.root_device.type
    if device_type == "cuda":
        torch.cuda.synchronize()
    elif device_type == "mps" and hasattr(torch.mps, "synchronize"):
        torch.mps.synchronize()


class EpochTimer(Callback):
    """Per-epoch wall-clock timer with device sync and warmup separation.

    Logged metrics (appear in WandB + CSV, keyed to the current epoch):
        train/epoch_wall_s    — end-to-end wall time (train batches + val pass)
                                NaN for warmup epoch so it never contaminates means
        train/compute_wall_s  — batch-compute-only time (sum of per-batch
                                forward+backward+optimizer, no data loading, no val)
        train/io_overhead_s   — epoch_wall_s − compute_wall_s
                                (data loading + val + overhead)
        train/warmup_wall_s   — same measurement for epoch 0, logged under a
                                separate key so it is visible but excluded from
                                epoch_wall_s statistics

    Printed summary format (one line per epoch):
        [warmup] wall=72.3s  compute=65.1s  io=7.2s
        [epoch 1] wall=61.4s  compute=55.8s  io=5.6s  (steady-state)
    """

    def __init__(self) -> None:
        self._epoch_wall_start: float | None = None
        self._batch_compute_start: float | None = None
        self._epoch_compute_s: float = 0.0
        self._is_warmup: bool = False

    # ── Epoch-level timing ────────────────────────────────────────────────────

    def on_train_epoch_start(self, trainer: Trainer, pl_module) -> None:
        self._is_warmup = trainer.current_epoch == 0
        self._epoch_compute_s = 0.0
        # Start the epoch wall clock AFTER the first batch is queued, so we
        # don't include Lightning's epoch-setup overhead. We start it here so
        # the first batch's data-loading is included in the IO term.
        self._epoch_wall_start = time.perf_counter()

    def on_validation_epoch_end(self, trainer: Trainer, pl_module) -> None:
        # Lightning fires this during the sanity check too — skip that.
        if trainer.sanity_checking or self._epoch_wall_start is None:
            return

        # Sync before reading the clock so all async ops are flushed.
        device_sync(trainer)
        epoch_s   = time.perf_counter() - self._epoch_wall_start
        compute_s = self._epoch_compute_s
        io_s      = max(0.0, epoch_s - compute_s)

        if self._is_warmup:
            pl_module.log("train/warmup_wall_s",  epoch_s,   on_epoch=True)
            label = "[warmup]"
        else:
            pl_module.log("train/epoch_wall_s",   epoch_s,   on_epoch=True)
            pl_module.log("train/compute_wall_s", compute_s, on_epoch=True)
            pl_module.log("train/io_overhead_s",  io_s,      on_epoch=True)
            label = f"[epoch {trainer.current_epoch}]"

        print(
            f"  {label:<12}  wall={epoch_s:.1f}s"
            f"  compute={compute_s:.1f}s  io={io_s:.1f}s"
        )

    # ── Batch-level compute timing ────────────────────────────────────────────

    def on_train_batch_start(self, trainer: Trainer, pl_module, batch, batch_idx: int) -> None:
        # Sync before starting the batch timer so any queued ops from the
        # previous batch don't bleed into this batch's measurement.
        device_sync(trainer)
        self._batch_compute_start = time.perf_counter()

    def on_train_batch_end(
        self, trainer: Trainer, pl_module, outputs, batch, batch_idx: int
    ) -> None:
        if self._batch_compute_start is None:
            return
        # Sync to flush the forward+backward+optimizer step before reading.
        device_sync(trainer)
        self._epoch_compute_s += time.perf_counter() - self._batch_compute_start
        self._batch_compute_start = None
