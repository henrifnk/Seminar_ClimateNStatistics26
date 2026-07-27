import csv
import json
from pathlib import Path
from typing import Any

import torch
import torch.nn as nn
from pytorch_lightning import LightningModule
from utils.conv_block import ConvBlock
from utils.metrics import drought_rmse_pooled, metrics_celled, metrics_celled_drought, metrics_celled_regression
from utils.plotting import make_heatmap  # noqa: E402


class ScaledTanh(nn.Module):
    def __init__(self, coef: int = 10):
        super().__init__()
        self.c = coef

    def forward(self, x):
        return torch.mul(torch.tanh(x), self.c)


class PinballLoss(nn.Module):
    """Quantile/pinball loss.

    Penalises under-prediction (ŷ < y) with weight q and over-prediction
    with weight (1-q).  q < 0.5 focuses the model on the lower tail, which
    is natural for drought (negative SPEI) because missing a drought
    (predicting too high) is penalised more heavily than a false alarm.
    """

    def __init__(self, quantile: float = 0.1):
        super().__init__()
        self.q = quantile

    def forward(self, preds: torch.Tensor, targets: torch.Tensor) -> torch.Tensor:
        errors = targets - preds
        return torch.max(self.q * errors, (self.q - 1) * errors).mean()


class CellwisePinballLoss(nn.Module):
    """Pinball loss with a per-cell quantile derived from local drought climatology.

    q[i,j] = P(SPEI[i,j] <= drought_threshold) over the training period, so
    drought-prone cells automatically receive a lower quantile (penalising
    missed droughts more) and rarely-dry cells receive a higher one.

    q_map_flat is registered as a buffer and set via set_q_map() in train.py
    once training data is available, then saved into the checkpoint automatically.
    """

    def __init__(self) -> None:
        super().__init__()
        self.register_buffer("q_map_flat", None)

    def set_q_map(self, q_map: torch.Tensor, mask: torch.Tensor) -> None:
        """q_map: (H, W) empirical drought frequency; mask: (H, W) bool."""
        self.q_map_flat = q_map[mask]  # (N_cells,)

    def forward(self, preds: torch.Tensor, targets: torch.Tensor) -> torch.Tensor:
        assert self.q_map_flat is not None, "call set_q_map() before training"
        errors = targets - preds        # (..., N_cells)
        q = self.q_map_flat             # (N_cells,) — broadcasts from the right
        return torch.max(q * errors, (q - 1) * errors).mean()


class StaticEncoder(nn.Module):
    """Single CNN that encodes static spatial fields (e.g. topography) once.

    The output is concatenated to [prev_h, x_emb] at every ConvLSTM timestep,
    giving the gates a spatially-aware, time-invariant conditioning signal.
    """

    def __init__(self, n_in: int, n_out: int, kernel_size: int = 3) -> None:
        super().__init__()
        pad = kernel_size // 2
        self.net = nn.Sequential(
            ConvBlock(n_in, n_out, kernel_size, stride=1, padding=pad),
            nn.ReLU(),
            ConvBlock(n_out, n_out, kernel_size, stride=1, padding=pad),
            nn.ReLU(),
        )

    def forward(self, static: torch.Tensor) -> torch.Tensor:
        return self.net(static)


class SeasonalStaticEncoder(nn.Module):
    """Four CNNs — one per season — hard-selected by the target month.

    DJF=0, MAM=1, JJA=2, SON=3.  All four encoders are run on the batch
    and the correct output is gathered per sample, keeping the forward pass
    fully batched without Python loops.
    """

    _MONTH_TO_SEASON: dict[int, int] = {
        12: 0, 1: 0, 2: 0,
        3: 1, 4: 1, 5: 1,
        6: 2, 7: 2, 8: 2,
        9: 3, 10: 3, 11: 3,
    }

    def __init__(self, n_in: int, n_out: int, kernel_size: int = 3) -> None:
        super().__init__()
        pad = kernel_size // 2
        self.encoders = nn.ModuleList([
            nn.Sequential(
                ConvBlock(n_in, n_out, kernel_size, stride=1, padding=pad),
                nn.ReLU(),
                ConvBlock(n_out, n_out, kernel_size, stride=1, padding=pad),
                nn.ReLU(),
            )
            for _ in range(4)
        ])

    def forward(self, static: torch.Tensor, months: torch.Tensor) -> torch.Tensor:
        # static:  (B, n_in, H, W)
        # months:  (B,) int64 — target month 1-12
        # returns: (B, n_out, H, W)
        all_enc = torch.stack([enc(static) for enc in self.encoders], dim=1)  # (B, 4, n_out, H, W)
        seasons = torch.tensor(
            [self._MONTH_TO_SEASON[m.item()] for m in months],
            device=static.device, dtype=torch.long,
        )
        n_out, H, W = all_enc.shape[2], all_enc.shape[3], all_enc.shape[4]
        idx = seasons[:, None, None, None, None].expand(-1, 1, n_out, H, W)
        return all_enc.gather(1, idx).squeeze(1)  # (B, n_out, H, W)


class FiLMGenerator(nn.Module):
    """MLP that maps global scalars → per-channel γ and β for FiLM conditioning.

    Applied to x_emb (embedding output) before the ConvLSTM gates:
        x_emb = γ · x_emb + β

    This lets the model modulate its spatial feature extraction based on
    large-scale climate indices (e.g. NAO, Mediterranean SST) at the most
    recent input timestep.
    """

    def __init__(self, n_scalars: int, n_channels: int, hidden_size: int = 64) -> None:
        super().__init__()
        self.net = nn.Sequential(
            nn.Linear(n_scalars, hidden_size),
            nn.ReLU(),
            nn.Linear(hidden_size, 2 * n_channels),
        )
        # Zero-init final layer so γ=0, β=0 at init; with 1+γ reparameterization
        # this gives identity-at-init: (1+0)·x + 0 = x.
        nn.init.zeros_(self.net[-1].weight)
        nn.init.zeros_(self.net[-1].bias)

    def forward(self, scalars: torch.Tensor) -> tuple[torch.Tensor, torch.Tensor]:
        # scalars: (B, n_scalars)
        out = self.net(scalars)                           # (B, 2 * n_channels)
        gamma, beta = out.chunk(2, dim=1)                 # each (B, n_channels)
        # 1 + γ reparameterization: identity-at-init, gradients flow through x
        return (1 + gamma).unsqueeze(-1).unsqueeze(-1), beta.unsqueeze(-1).unsqueeze(-1)


class WeightedMSELoss(nn.Module):
    """MSE with per-cell weights that upweight negative SPEI values.

    mode='threshold': binary — cells below drought_threshold receive
        drought_weight, all others receive 1.
    mode='absolute': weight = |target|, scales with severity on both tails
        (analogous to Ravuri et al. 2021 intensity weighting).
    mode='hinge': weight = 1 + drought_weight * max(0, -target) — flat at 1
        for positive SPEI, linearly increasing for negative SPEI. Only the dry
        tail is upweighted. drought_weight controls the slope (typical: 1–2;
        at slope=1 and SPEI=-1.5 → weight=2.5, at SPEI=-2.0 → weight=3.0).
    """

    def __init__(
        self,
        drought_threshold: float = -1.5,
        drought_weight: float = 5.0,
        mode: str = "threshold",
    ):
        super().__init__()
        if mode not in ("absolute", "threshold", "hinge"):
            raise ValueError(f"mode must be 'absolute', 'threshold', or 'hinge', got '{mode}'")
        self.threshold = drought_threshold
        self.drought_weight = drought_weight
        self.mode = mode

    def forward(self, preds: torch.Tensor, targets: torch.Tensor) -> torch.Tensor:
        sq_err = (preds - targets) ** 2
        if self.mode == "absolute":
            weights = targets.abs()
        elif self.mode == "hinge":
            weights = 1.0 + self.drought_weight * (-targets).clamp(min=0)
        else:
            weights = torch.where(
                targets < self.threshold,
                torch.full_like(targets, self.drought_weight),
                torch.ones_like(targets),
            )
        return (weights * sq_err).mean()


class RCNNModule(LightningModule):
    def __init__(
        self,
        mode: str = "regression",
        embedding_size: int = 16,
        hidden_state_size: int = 32,
        kernel_size: int = 3,
        groups: int = 1,
        dilation: int = 1,
        n_cells_hor: int = 44,  # rlat (padded)
        n_cells_ver: int = 98,  # rlon
        history_length: int = 12,
        periods_forward: int = 1,  # how many timesteps to predict simultaniously
        batch_size: int = 1,
        num_of_additional_features: int = 4,  # wb, pr, ps, tas
        boundaries: list[int] = [-2],
        num_classes: int = 2,
        values_range: int = 10,
        dropout: float = 0.0,
        drought_threshold: float = -1.5,
        loss_fn: str = "mse",
        pinball_quantile: float = 0.1,
        drought_weight: float = 5.0,
        weighted_loss_mode: str = "threshold",
        static_encoder: str = "none",   # none | single | seasonal
        static_emb_size: int = 16,
        n_static_in: int = 2,
        global_encoder: str = "none",   # none | film | naive
        n_film_scalars: int = 0,
        n_extra_channels: int = 0,      # naive-injected channels (static + global scalars)
        film_hidden_size: int = 64,
        lr: float = 0.003,
        weight_decay: float = 0.0,
        lr_scheduler_patience: int = 5,
    ):
        super().__init__()
        self.save_hyperparameters(logger=False)

        self.n_cells_hor = n_cells_hor
        self.n_cells_ver = n_cells_ver
        self.history_length = history_length
        self.periods_forward = periods_forward
        self.batch_size = batch_size
        self.lr = lr
        self.weight_decay = weight_decay

        self.num_of_features = num_of_additional_features + 1  # +1 for SPEI (target/autoregressive channel)
        self.tanh_coef = values_range
        self.dropout = nn.Dropout2d(p=dropout)
        self.num_class = num_classes

        self.emb_size = embedding_size
        self.hid_size = hidden_state_size
        self.kernel_size = kernel_size
        self.dilation = dilation
        self.groups = groups

        self.saved_predictions = None
        self.saved_targets = None
        # global_avg and mask are registered as buffers so they are saved in
        # checkpoints and moved to the correct device automatically.
        # Both are set to their real values in train.py via register_buffer().
        self.register_buffer("global_avg", None)
        self.register_buffer("mask", torch.ones(n_cells_hor, n_cells_ver, dtype=torch.bool))
        # Per-cell linear trend predictions for the test period.
        # Set in train.py after fitting OLS on training targets.
        self.register_buffer("trend_test", None)

        # accumulated across steps; cleared in on_*_epoch_end
        self._train_outputs: list[dict] = []
        self._val_outputs: list[dict] = []
        self._test_outputs: list[dict] = []

        self.register_buffer("boundaries", torch.tensor(boundaries, dtype=torch.float32))

        # Static spatial encoder (optional)
        self.static_encoder_mode = static_encoder
        self._static_emb = static_emb_size if static_encoder in ("single", "seasonal") else 0
        self.register_buffer("static_map", None)  # set in train.py from dataset.static_features
        if static_encoder == "single":
            self.static_enc: nn.Module = StaticEncoder(n_static_in, static_emb_size, kernel_size)
        elif static_encoder == "seasonal":
            self.static_enc = SeasonalStaticEncoder(n_static_in, static_emb_size, kernel_size)

        # Global scalar FiLM conditioning (optional)
        self.global_encoder_mode = global_encoder
        if global_encoder == "film":
            if n_film_scalars < 1:
                raise ValueError("global_encoder='film' requires n_film_scalars >= 1")
            self.film_gen = FiLMGenerator(n_film_scalars, embedding_size, film_hidden_size)

        self.embedding = nn.Sequential(
            ConvBlock(
                self.num_of_features + n_extra_channels,
                self.emb_size,
                self.kernel_size,
                stride=1,
                padding=self.kernel_size // 2,
                dilation=self.dilation,
                groups=self.groups,
            ),
            nn.ReLU(),
            ConvBlock(
                self.emb_size,
                self.emb_size,
                self.kernel_size,
                stride=1,
                padding=self.kernel_size // 2,
            ),
        )

        _lstm_in = self.hid_size + self.emb_size + self._static_emb
        _pad = self.kernel_size // 2
        self.f_t = nn.Sequential(
            ConvBlock(_lstm_in, self.hid_size, self.kernel_size, stride=1, padding=_pad),
            nn.Sigmoid(),
        )
        self.i_t = nn.Sequential(
            ConvBlock(_lstm_in, self.hid_size, self.kernel_size, stride=1, padding=_pad),
            nn.Sigmoid(),
        )
        self.c_t = nn.Sequential(
            ConvBlock(_lstm_in, self.hid_size, self.kernel_size, stride=1, padding=_pad),
            nn.Tanh(),
        )
        self.o_t = nn.Sequential(
            ConvBlock(_lstm_in, self.hid_size, self.kernel_size, stride=1, padding=_pad),
            nn.Sigmoid(),
        )

        self.final_conv = nn.Sequential(
            nn.Conv2d(self.hid_size, self.periods_forward, kernel_size=1, bias=False),
            ScaledTanh(self.tanh_coef),
        )

        self.final_classify = nn.Sequential(
            ConvBlock(self.hid_size, self.num_class, kernel_size=3, stride=1, padding=1),
            nn.Sigmoid(),
        )

        self.mode = mode
        if self.mode == "regression":
            if loss_fn == "mse":
                self.criterion = nn.MSELoss()
            elif loss_fn == "pinball":
                self.criterion = PinballLoss(quantile=pinball_quantile)
            elif loss_fn == "cellwise_pinball":
                self.criterion = CellwisePinballLoss()
            elif loss_fn == "weighted_mse":
                self.criterion = WeightedMSELoss(
                    drought_threshold=drought_threshold,
                    drought_weight=drought_weight,
                    mode=weighted_loss_mode,
                )
            else:
                raise ValueError(
                    f"Unknown loss_fn '{loss_fn}'. "
                    "Choose 'mse', 'pinball', 'cellwise_pinball', or 'weighted_mse'."
                )
        else:
            self.criterion = nn.CrossEntropyLoss()

    def forward(
        self,
        x: torch.Tensor,
        months: torch.Tensor | None = None,
        scalars: torch.Tensor | None = None,
    ) -> torch.Tensor:
        # x:       (B, T, n_vars, H, W)  T = history_length
        # months:  (B,) target month 1–12 — used by seasonal static encoder
        # scalars: (B, T, n_global)       per-step global scalars for FiLM
        B, T, _, H, W = x.shape

        # ── Static encoding: computed once per forward, reused every step ───
        # Stays on the gradient path in training (computed fresh every forward).
        static_feat = None
        if self.static_encoder_mode != "none" and self.static_map is not None:
            static_in = self.static_map.unsqueeze(0).expand(B, -1, -1, -1)
            if self.static_encoder_mode == "single":
                static_feat = self.static_enc(static_in)           # (B, static_emb, H, W)
            else:  # seasonal: one sub-encoder selected by target month
                static_feat = self.static_enc(static_in, months)   # (B, static_emb, H, W)

        # ── Per-forward zero state — no cross-sample carry ──────────────────
        h = x.new_zeros(B, self.hid_size, H, W)
        c = x.new_zeros(B, self.hid_size, H, W)

        # ── Unroll over T timesteps (many-to-one: output read at t=T-1) ────
        for t in range(T):
            x_t = self.dropout(x[:, t])          # (B, n_vars, H, W)
            x_emb = self.embedding(x_t)          # (B, emb_size, H, W)

            # Per-step FiLM: γ_t, β_t from this step's own scalars
            if self.global_encoder_mode == "film" and scalars is not None:
                gamma, beta = self.film_gen(scalars[:, t])  # (B, n_global) → γ,β
                x_emb = gamma * x_emb + beta

            if static_feat is not None:
                gate_in = torch.cat([h, x_emb, static_feat], dim=1)
            else:
                gate_in = torch.cat([h, x_emb], dim=1)

            f = self.f_t(gate_in)
            i = self.i_t(gate_in)
            g = self.c_t(gate_in)
            o = self.o_t(gate_in)

            c = c * f + i * g
            h = torch.tanh(c) * o

        # Many-to-one: prediction from final hidden state only
        if self.mode == "regression":
            return self.final_conv(h)      # (B, periods_forward, H, W)
        else:
            return self.final_classify(h)  # (B, num_classes, H, W)

    def step(self, batch: Any) -> tuple[torch.Tensor, torch.Tensor, torch.Tensor]:
        months = scalars = None
        if len(batch) == 4:
            x, y, months, scalars = batch
        elif len(batch) == 3:
            x, y = batch[0], batch[1]
            if batch[2].ndim == 1:   # months: (B,) — from MonthAwareSubset
                months = batch[2]
            else:                     # scalars: (B, n_global) — from ScalarSubset
                scalars = batch[2]
        else:
            x, y = batch
        preds = self.forward(x, months=months, scalars=scalars)  # (B, periods_forward, H, W)

        if self.mode == "regression":
            # compute loss over valid Alpine cells only
            loss = self.criterion(preds[:, :, self.mask], y[:, :, self.mask])
            preds_out = preds[:, -1]  # (B, H, W) — last (only) horizon
            target_out = y[:, -1]  # (B, H, W)
        else:
            target_out = y[:, -1].long()
            loss = self.criterion(preds, target_out)
            preds_out = preds

        return loss, preds_out, target_out

    def regression_baseline(self, batch: Any) -> torch.Tensor:
        x = batch[0]  # (B, T, n_vars, H, W)
        # persistence: last observed SPEI — final timestep, channel 0
        return x[:, -1, 0]  # (B, H, W)

    def class_baseline(self, batch: Any) -> torch.Tensor:
        x = batch[0]  # (B, T, n_vars, H, W)
        spei_hist = x[:, :, 0]   # (B, T, H, W) — SPEI channel across all timesteps
        x_binned = torch.bucketize(spei_hist, self.boundaries)
        most_freq_values, _ = torch.mode(x_binned, dim=1)
        return most_freq_values

    def _log_dir(self) -> Path:
        """Resolve the logger's output directory, falling back to logs/."""
        log_dir_str = getattr(self.logger, "log_dir", None) if self.logger else None
        return Path(log_dir_str) if log_dir_str else Path("logs")

    def _write_test_metrics(self, metrics: dict[str, float]) -> None:
        log_dir = self._log_dir()
        log_dir.mkdir(parents=True, exist_ok=True)
        path = log_dir / "test_metrics.json"
        with open(path, "w") as f:
            json.dump(metrics, f, indent=2)
        print(f"  test metrics → {path}")

    def _write_epoch_log(self, row: dict[str, float]) -> None:
        """Append one row to a per-epoch CSV so training curves are easy to load."""
        log_dir = self._log_dir()
        log_dir.mkdir(parents=True, exist_ok=True)
        path = log_dir / "epoch_metrics.csv"
        write_header = not path.exists()
        with open(path, "a", newline="") as f:
            writer = csv.DictWriter(f, fieldnames=list(row.keys()))
            if write_header:
                writer.writeheader()
            writer.writerow(row)

    # ------------------------------------------------------------------
    # Training
    # ------------------------------------------------------------------

    def training_step(self, batch: Any, batch_idx: int) -> dict:
        loss, preds, targets = self.step(batch)
        self.log("train/loss", loss, on_step=False, on_epoch=True, prog_bar=True)
        out = {"loss": loss, "preds": preds, "targets": targets}
        self._train_outputs.append(out)
        return out

    def on_train_epoch_end(self) -> None:
        train_loss = torch.stack([o["loss"] for o in self._train_outputs]).mean()
        all_preds = torch.cat([o["preds"] for o in self._train_outputs])
        all_targets = torch.cat([o["targets"] for o in self._train_outputs])
        self._train_outputs.clear()

        if self.mode == "regression":
            rmse, mae, corr = metrics_celled_regression(all_targets, all_preds)
            tr_rmse = torch.median(rmse[self.mask])
            tr_mae = torch.median(mae[self.mask])
            tr_corr = torch.median(corr[self.mask])
            self.log("train/rmse_median", tr_rmse, prog_bar=True)
            self.log("train/mae_median", tr_mae, prog_bar=True)
            self.log("train/corr_median", tr_corr)

            acc_d, f1_d, _, _tpr = metrics_celled_drought(all_targets, all_preds, self.hparams.drought_threshold)
            tr_f1_d = torch.median(f1_d[self.mask])
            tr_acc_d = torch.median(acc_d[self.mask])
            self.log("train/drought_f1_median", tr_f1_d)
            self.log("train/drought_acc_median", tr_acc_d)

            self._train_summary = {
                "loss": train_loss.item(),
                "rmse": tr_rmse.item(),
                "mae": tr_mae.item(),
                "corr": tr_corr.item(),
                "drought_f1": tr_f1_d.item(),
                "drought_acc": tr_acc_d.item(),
            }
        else:
            all_preds = torch.softmax(all_preds, dim=1)[:, 1]
            roc, ap, f1, acc, _ = metrics_celled(all_targets, all_preds)
            tr_f1 = torch.median(f1[self.mask])
            tr_roc = torch.median(roc[self.mask])
            self.log("train/f1_median", tr_f1, prog_bar=True)
            self.log("train/ap_median", torch.median(ap[self.mask]))
            self.log("train/rocauc_median", tr_roc, prog_bar=True)
            self._train_summary = {"loss": train_loss.item(), "f1": tr_f1.item(), "rocauc": tr_roc.item()}

    # ------------------------------------------------------------------
    # Validation
    # ------------------------------------------------------------------

    def validation_step(self, batch: Any, batch_idx: int) -> dict:
        loss, preds, targets = self.step(batch)
        self.log("val/loss", loss, on_step=False, on_epoch=True, prog_bar=True)
        out = {"loss": loss, "preds": preds, "targets": targets}
        self._val_outputs.append(out)
        return out

    def on_validation_epoch_end(self) -> None:
        all_preds = torch.cat([o["preds"] for o in self._val_outputs])
        all_targets = torch.cat([o["targets"] for o in self._val_outputs])
        self._val_outputs.clear()

        tr = getattr(self, "_train_summary", {})

        if self.mode == "regression":
            rmse, mae, corr = metrics_celled_regression(all_targets, all_preds)
            val_rmse = torch.median(rmse[self.mask])
            val_mae = torch.median(mae[self.mask])
            val_corr = torch.median(corr[self.mask])
            self.log("epoch", float(self.current_epoch))
            self.log("val/rmse_median", val_rmse, prog_bar=True)
            self.log("val/mae_median", val_mae, prog_bar=True)
            self.log("val/corr_median", val_corr)

            acc_d, f1_d, roc_d, _tpr = metrics_celled_drought(
                all_targets, all_preds, self.hparams.drought_threshold, compute_roc=True
            )
            assert roc_d is not None
            val_f1_d = torch.median(f1_d[self.mask])
            val_acc_d = torch.median(acc_d[self.mask])
            _roc_v = roc_d[self.mask]
            val_rocauc_d = _roc_v[_roc_v.isnan().logical_not()].median()
            self.log("val/drought_f1_median", val_f1_d, prog_bar=True)
            self.log("val/drought_acc_median", val_acc_d)
            self.log("val/drought_rocauc_median", val_rocauc_d, prog_bar=True)

            # Pooled drought F1: single global confusion matrix across all cells × timesteps.
            # Used as the pre-registered loss-selection criterion (Phase 1).
            thr = self.hparams.drought_threshold
            m = self.mask
            bin_t_p = (all_targets <= thr)[:, m].reshape(-1)
            bin_p_p = (all_preds   <= thr)[:, m].reshape(-1)
            tp_p = (bin_p_p & bin_t_p).float().sum()
            fp_p = (bin_p_p & bin_t_p.logical_not()).float().sum()
            fn_p = (bin_p_p.logical_not() & bin_t_p).float().sum()
            prec_p = tp_p / (tp_p + fp_p).clamp(min=1e-8)
            rec_p  = tp_p / (tp_p + fn_p).clamp(min=1e-8)
            val_f1_pooled = 2 * prec_p * rec_p / (prec_p + rec_p).clamp(min=1e-8)
            self.log("val/drought_f1_pooled", val_f1_pooled, prog_bar=True)
            self.log("val/drought_rmse_pooled", drought_rmse_pooled(all_targets, all_preds, m, thr))  # type: ignore[arg-type]

            # Climatology RMSE on val set (sanity floor for loss selection).
            val_clim_rmse = torch.tensor(float("nan"), device=all_targets.device)
            if self.global_avg is not None:
                clim_v = self.global_avg.to(all_targets.device).unsqueeze(0).expand_as(all_targets).float()
                rmse_cv, _, _ = metrics_celled_regression(all_targets, clim_v)
                val_clim_rmse = torch.median(rmse_cv[m])
                self.log("val/clim/rmse_median", val_clim_rmse)

            if tr:  # skip during Lightning's sanity check (no training epoch yet)
                self._write_epoch_log(
                    {
                        "epoch": self.current_epoch,
                        "train_loss": tr.get("loss", float("nan")),
                        "train_rmse": tr.get("rmse", float("nan")),
                        "train_mae": tr.get("mae", float("nan")),
                        "train_corr": tr.get("corr", float("nan")),
                        "train_drought_f1": tr.get("drought_f1", float("nan")),
                        "train_drought_acc": tr.get("drought_acc", float("nan")),
                        "val_rmse": val_rmse.item(),
                        "val_mae": val_mae.item(),
                        "val_corr": val_corr.item(),
                        "val_drought_f1": val_f1_d.item(),
                        "val_drought_acc": val_acc_d.item(),
                        "val_drought_rocauc": val_rocauc_d.item(),
                        "val_drought_f1_pooled": val_f1_pooled.item(),
                        "val_clim_rmse": val_clim_rmse.item(),
                    }
                )

            if tr:
                print(
                    f"\n  Epoch {self.current_epoch:3d} | "
                    f"train  loss={tr.get('loss', float('nan')):.4f}  "
                    f"rmse={tr.get('rmse', float('nan')):.3f}  "
                    f"corr={tr.get('corr', float('nan')):.3f}  "
                    f"drought_f1={tr.get('drought_f1', float('nan')):.3f} | "
                    f"val    rmse={val_rmse:.3f}  corr={val_corr:.3f}  "
                    f"drought_f1={val_f1_d:.3f}  drought_rocauc={val_rocauc_d:.3f}"
                )
        else:
            all_preds = torch.softmax(all_preds, dim=1)[:, 1]
            roc, ap, f1, acc, _ = metrics_celled(all_targets, all_preds)
            val_f1 = torch.median(f1[self.mask])
            val_roc = torch.median(roc[self.mask])
            self.log("val/f1_median", val_f1, prog_bar=True)
            self.log("val/ap_median", torch.median(ap[self.mask]))
            self.log("val/rocauc_median", val_roc, prog_bar=True)

            print(
                f"\n  Epoch {self.current_epoch:3d} | "
                f"train  loss={tr.get('loss', float('nan')):.4f}  "
                f"f1={tr.get('f1', float('nan')):.3f}  "
                f"rocauc={tr.get('rocauc', float('nan')):.3f} | "
                f"val    f1={val_f1:.3f}  rocauc={val_roc:.3f}"
            )

    # ------------------------------------------------------------------
    # Test
    # ------------------------------------------------------------------

    def test_step(self, batch: Any, batch_idx: int) -> dict:
        loss, preds, targets = self.step(batch)
        if self.mode == "regression":
            baseline = self.regression_baseline(batch)
        else:
            baseline = self.class_baseline(batch)
        self.log("test/loss", loss, on_step=False, on_epoch=True, prog_bar=True)
        out = {"loss": loss, "preds": preds, "targets": targets, "baseline": baseline}
        self._test_outputs.append(out)
        return out

    def on_test_epoch_end(self) -> None:
        all_preds = torch.cat([o["preds"] for o in self._test_outputs])
        all_targets = torch.cat([o["targets"] for o in self._test_outputs])
        all_baselines = torch.cat([o["baseline"] for o in self._test_outputs])
        self._test_outputs.clear()

        init_len = all_baselines.shape[0]
        all_targets = all_targets[:init_len]
        self.saved_targets = all_targets

        if self.mode == "regression":
            all_preds = all_preds[:init_len]
            all_baselines = all_baselines[:init_len]
            self.saved_predictions = all_preds

            assert self.global_avg is not None, "set global_avg before testing"
            clim = self.global_avg.to(all_targets.device).unsqueeze(0).expand_as(all_targets).float()

            rmse, mae, corr = metrics_celled_regression(all_targets, all_preds)
            rmse_p, mae_p, corr_p = metrics_celled_regression(all_targets, all_baselines)
            rmse_c, mae_c, corr_c = metrics_celled_regression(all_targets, clim)

            log_dir = self._log_dir()
            _rlat = self.rlat.cpu().numpy() if hasattr(self, "rlat") else None
            _rlon = self.rlon.cpu().numpy() if hasattr(self, "rlon") else None
            rmse_path = make_heatmap(
                rmse.cpu(),
                path=log_dir / "test_rmse_spatial.png",
                mask=self.mask,
                rlat=_rlat,
                rlon=_rlon,
                title="Test RMSE (Alpine cells)",
                cmap="Reds",
            )
            corr_path = make_heatmap(
                corr.cpu(),
                path=log_dir / "test_corr_spatial.png",
                mask=self.mask,
                rlat=_rlat,
                rlon=_rlon,
                vmin=-1.0,
                vmax=1.0,
                title="Test correlation (Alpine cells)",
                cmap="RdYlBu_r",
            )
            acc_d, f1_d, roc_d, tpr_d = metrics_celled_drought(
                all_targets, all_preds, self.hparams.drought_threshold, compute_roc=True
            )
            assert roc_d is not None
            acc_path = make_heatmap(
                acc_d.cpu(),
                path=log_dir / "test_drought_acc_spatial.png",
                mask=self.mask,
                rlat=_rlat,
                rlon=_rlon,
                vmin=0.0,
                vmax=1.0,
                title=f"Test drought accuracy (SPEI≤{self.hparams.drought_threshold})",
                cmap="RdYlGn",
            )
            f1_path = make_heatmap(
                f1_d.cpu(),
                path=log_dir / "test_drought_f1_spatial.png",
                mask=self.mask,
                rlat=_rlat,
                rlon=_rlon,
                vmin=0.0,
                vmax=1.0,
                title=f"Test drought F1 (SPEI≤{self.hparams.drought_threshold})",
                cmap="RdYlGn",
            )
            tpr_path = make_heatmap(
                tpr_d.cpu(),
                path=log_dir / "test_drought_tpr_spatial.png",
                mask=self.mask,
                rlat=_rlat,
                rlon=_rlon,
                vmin=0.0,
                vmax=1.0,
                title=f"Test drought TPR / recall (SPEI≤{self.hparams.drought_threshold})",
                cmap="RdYlGn",
            )
            if self.logger and hasattr(self.logger, "log_image"):
                self.logger.log_image("test/rmse_spatial", [rmse_path])
                self.logger.log_image("test/corr_spatial", [corr_path])
                self.logger.log_image("test/drought_acc_spatial", [acc_path])
                self.logger.log_image("test/drought_f1_spatial", [f1_path])
                self.logger.log_image("test/drought_tpr_spatial", [tpr_path])

            m      = self.mask               # (H, W) bool mask of valid Alpine cells
            T_test = all_targets.shape[0]
            n_valid_cells       = int(m.sum().item())
            n_valid_cell_months = T_test * n_valid_cells

            thr = self.hparams.drought_threshold

            # ── Pooled scalar metrics (all valid cell-months as one pool) ─────────────
            # Flatten to 1D: (T_test × n_valid_cells,)
            tgt_flat  = all_targets[:, m].reshape(-1)
            pred_flat = all_preds[:, m].reshape(-1)

            rmse_pooled = torch.sqrt(((pred_flat - tgt_flat) ** 2).mean())
            mae_pooled  = (pred_flat - tgt_flat).abs().mean()
            _pc = pred_flat - pred_flat.mean()
            _tc = tgt_flat  - tgt_flat.mean()
            corr_pooled = (
                (_pc * _tc).sum()
                / torch.sqrt((_pc**2).sum() * (_tc**2).sum()).clamp(min=1e-8)
            ).clamp(-1.0, 1.0)

            # Pooled baseline RMSEs and drought metrics
            persist_flat        = all_baselines[:, m].reshape(-1)
            clim_flat           = clim[:, m].reshape(-1)
            rmse_persist_pooled = torch.sqrt(((persist_flat - tgt_flat) ** 2).mean())
            rmse_clim_pooled    = torch.sqrt(((clim_flat    - tgt_flat) ** 2).mean())

            def _pooled_drought_pf(pf, tf):
                _bt = tf <= thr
                _bp = pf <= thr
                _tp = (_bp & _bt).float().sum()
                _fp = (_bp & ~_bt).float().sum()
                _fn = (~_bp & _bt).float().sum()
                _pr = _tp / (_tp + _fp).clamp(min=1e-8)
                _rc = _tp / (_tp + _fn).clamp(min=1e-8)
                return _rc, 2 * _pr * _rc / (_pr + _rc).clamp(min=1e-8)  # tpr, f1

            tpr_pooled,          f1_pooled          = _pooled_drought_pf(pred_flat,    tgt_flat)
            persist_tpr_pooled,  persist_f1_pooled  = _pooled_drought_pf(persist_flat, tgt_flat)
            clim_tpr_pooled,     clim_f1_pooled     = _pooled_drought_pf(clim_flat,    tgt_flat)

            # Trend baseline pooled metrics (full per-cell eval lives in eval_baselines.py)
            _nan = torch.tensor(float("nan"), device=all_targets.device)
            trend_rmse_pooled = trend_tpr_pooled = trend_f1_pooled = _nan
            if self.trend_test is not None:
                trend_all         = self.trend_test.to(all_targets.device)[: T_test]
                trend_flat        = trend_all[:, m].reshape(-1)
                trend_rmse_pooled = torch.sqrt(((trend_flat - tgt_flat) ** 2).mean())
                trend_tpr_pooled, trend_f1_pooled = _pooled_drought_pf(trend_flat, tgt_flat)

            rmse_vs_trend = trend_rmse_pooled - rmse_pooled

            print(
                f"  Pooled metric base: {n_valid_cell_months:,} valid cell-months "
                f"({n_valid_cells} cells × {T_test} test months)"
            )

            # ── Log pooled metrics ────────────────────────────────────────────────────
            self.log("test/rmse_pooled",                    rmse_pooled)
            self.log("test/mae_pooled",                     mae_pooled)
            self.log("test/corr_pooled",                    corr_pooled)
            self.log("test/drought_tpr_pooled",             tpr_pooled)
            self.log("test/drought_f1_pooled",              f1_pooled)
            self.log("test/drought_rmse_pooled",            drought_rmse_pooled(all_targets, all_preds, m, thr))  # type: ignore[arg-type]
            self.log("test/persistence/rmse_pooled",        rmse_persist_pooled)
            self.log("test/persistence/drought_tpr_pooled", persist_tpr_pooled)
            self.log("test/persistence/drought_f1_pooled",  persist_f1_pooled)
            self.log("test/clim/rmse_pooled",               rmse_clim_pooled)
            self.log("test/clim/drought_tpr_pooled",        clim_tpr_pooled)
            self.log("test/clim/drought_f1_pooled",         clim_f1_pooled)
            self.log("test/trend/rmse_pooled",              trend_rmse_pooled)
            self.log("test/trend/drought_tpr_pooled",       trend_tpr_pooled)
            self.log("test/trend/drought_f1_pooled",        trend_f1_pooled)
            self.log("test/rmse_vs_trend",                  rmse_vs_trend)

            # ── Per-cell median metrics (model only) ──────────────────────────────────
            # Baseline per-cell medians and figures are computed once in eval_baselines.py
            def _tpr_med(tpr_arr):
                v = tpr_arr[m]
                return v[v.isnan().logical_not()].median()

            tpr_median = _tpr_med(tpr_d)
            roc_valid  = roc_d[m]
            roc_median = roc_valid[roc_valid.isnan().logical_not()].median()

            self.log("test/rmse_median",          torch.median(rmse[m]))
            self.log("test/mae_median",            torch.median(mae[m]))
            self.log("test/corr_median",           torch.median(corr[m]))
            self.log("test/drought_f1_median",     torch.median(f1_d[m]))
            self.log("test/drought_acc_median",    torch.median(acc_d[m]))
            self.log("test/drought_rocauc_median", roc_median)
            self.log("test/drought_tpr_median",    tpr_median)
            self.log("test/persistence/rmse_median", torch.median(rmse_p[m]))
            self.log("test/clim/rmse_median",        torch.median(rmse_c[m]))

            # ── Residual floor (test-period detrended std — data property, not a metric) ──
            test_std       = all_targets.std(dim=0)   # (H, W)
            residual_floor = torch.median(test_std[m])
            skill_margin   = residual_floor - torch.median(rmse[m])
            self.log("test/residual_floor", residual_floor)
            self.log("test/skill_margin",   skill_margin)

            self._write_test_metrics(
                {
                    # Pooled (headline)
                    "rmse_pooled":                    rmse_pooled.item(),
                    "mae_pooled":                     mae_pooled.item(),
                    "corr_pooled":                    corr_pooled.item(),
                    "drought_f1_pooled":              f1_pooled.item(),
                    "drought_tpr_pooled":             tpr_pooled.item(),
                    "persistence_rmse_pooled":        rmse_persist_pooled.item(),
                    "persistence_drought_tpr_pooled": persist_tpr_pooled.item(),
                    "persistence_drought_f1_pooled":  persist_f1_pooled.item(),
                    "clim_rmse_pooled":               rmse_clim_pooled.item(),
                    "clim_drought_tpr_pooled":        clim_tpr_pooled.item(),
                    "clim_drought_f1_pooled":         clim_f1_pooled.item(),
                    "trend_rmse_pooled":              trend_rmse_pooled.item(),
                    "trend_drought_tpr_pooled":       trend_tpr_pooled.item(),
                    "trend_drought_f1_pooled":        trend_f1_pooled.item(),
                    "rmse_vs_trend":                  rmse_vs_trend.item(),
                    "valid_cell_months":              n_valid_cell_months,
                    # Per-cell median (model only; baselines → figures/baselines/)
                    "rmse_median":          torch.median(rmse[m]).item(),
                    "mae_median":           torch.median(mae[m]).item(),
                    "corr_median":          torch.median(corr[m]).item(),
                    "drought_f1_median":    torch.median(f1_d[m]).item(),
                    "drought_acc_median":   torch.median(acc_d[m]).item(),
                    "drought_rocauc_median": roc_median.item(),
                    "drought_tpr_median":   tpr_median.item(),
                    "persistence_rmse_median": torch.median(rmse_p[m]).item(),
                    "clim_rmse_median":        torch.median(rmse_c[m]).item(),
                    "residual_floor":       residual_floor.item(),
                    "skill_margin":         skill_margin.item(),
                }
            )
        else:
            all_preds = all_preds[:init_len]
            all_preds = torch.softmax(all_preds, dim=1)[:, 1]
            self.saved_predictions = all_preds

            assert self.global_avg is not None, "set global_avg before testing"
            clim_base = self.global_avg.to(all_targets.device).unsqueeze(0).expand(len(all_targets), -1, -1)
            zeros = torch.zeros_like(all_preds)

            roc, ap, f1, acc, _ = metrics_celled(all_targets, all_preds, "test")
            roc_z, ap_z, f1_z, acc_z, _ = metrics_celled(all_targets, zeros, "test")
            roc_g, ap_g, f1_g, acc_g, _ = metrics_celled(all_targets, clim_base.double(), "test")

            log_dir = self._log_dir()
            roc_path = make_heatmap(
                roc, path=log_dir / "test_rocauc_spatial.png", mask=self.mask, title="Test ROCAUC (Alpine cells)"
            )
            if self.logger and hasattr(self.logger, "log_image"):
                self.logger.log_image("test/rocauc_spatial", [roc_path])

            self.log("test/f1_median", torch.median(f1))
            self.log("test/rocauc_median", torch.median(roc))
            self.log("test/global/rocauc_median", torch.median(roc_g))
            self.log("test/zeros/rocauc_median", torch.median(roc_z))

            self._write_test_metrics(
                {
                    "f1_median": torch.median(f1).item(),
                    "rocauc_median": torch.median(roc).item(),
                    "global_rocauc_median": torch.median(roc_g).item(),
                    "zeros_rocauc_median": torch.median(roc_z).item(),
                }
            )

    def configure_optimizers(self):
        opt = torch.optim.Adam(self.parameters(), lr=self.hparams.lr, weight_decay=self.hparams.weight_decay)
        scheduler = torch.optim.lr_scheduler.ReduceLROnPlateau(
            opt,
            mode="min",
            factor=0.5,
            patience=self.hparams.lr_scheduler_patience,
            min_lr=1e-5,
        )
        return {"optimizer": opt, "lr_scheduler": {"scheduler": scheduler, "monitor": "val/loss"}}
