import logging
import shutil
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent))
sys.stdout.reconfigure(encoding="utf-8")
sys.stderr.reconfigure(encoding="utf-8")

# triton is Linux/CUDA-compiler-only; this warning is a no-op on our setup.
logging.getLogger("torch.utils.flop_counter").setLevel(logging.ERROR)

import hydra  # noqa: E402
import numpy as np  # noqa: E402
import torch  # noqa: E402
from dataset import DroughtDataset, MonthAwareSubset, MonthScalarSubset, ScalarSubset  # noqa: E402
from hydra.core.hydra_config import HydraConfig  # noqa: E402
from model import RCNNModule  # noqa: E402
from omegaconf import DictConfig, OmegaConf  # noqa: E402
from pytorch_lightning import Trainer, seed_everything  # noqa: E402
from pytorch_lightning.callbacks import EarlyStopping, ModelCheckpoint  # noqa: E402
from torch.utils.data import DataLoader  # noqa: E402
from utils.timing import EpochTimer  # noqa: E402


def _model_tag(cfg: DictConfig) -> str:
    """Build a descriptive filename tag from the active model config."""
    if cfg.get("run_name"):
        return cfg.run_name
    parts = [cfg.model.mode, cfg.model.loss_fn]
    if cfg.model.loss_fn == "pinball":
        parts.append(f"q{cfg.model.pinball_quantile:.2f}")
    elif cfg.model.loss_fn == "cellwise_pinball":
        pass  # quantile is data-derived; loss name is already descriptive
    elif cfg.model.loss_fn == "weighted_mse":
        parts.append(cfg.model.weighted_loss_mode)
        parts.append(f"w{cfg.model.drought_weight}")
    if cfg.model.static_encoder != "none":
        parts.append(cfg.model.static_encoder)
    if cfg.model.global_encoder != "none":
        parts.append(cfg.model.global_encoder)
        if cfg.data.med_sst_agg != "grouped":
            parts.append(f"sst_{cfg.data.med_sst_agg}")
    parts.append(f"lead{cfg.data.lead_time}")
    parts.append(f"h{cfg.model.hidden_state_size}")
    parts.append(f"hist{cfg.model.history_length}")
    return "_".join(str(p) for p in parts)


def _build_logger(cfg: DictConfig):
    if cfg.logger.name == "wandb":
        from pytorch_lightning.loggers import WandbLogger
        return WandbLogger(
            project=cfg.logger.project,
            entity=cfg.logger.entity or None,
            name=cfg.logger.run_name or None,
            group=getattr(cfg.logger, "group", None) or None,
            tags=list(cfg.logger.tags),
            log_model=cfg.logger.log_model,
            # Log the full resolved config so every run is reproducible
            config=OmegaConf.to_container(cfg, resolve=True),
        )
    return True  # Lightning default CSVLogger


@hydra.main(config_path="../configs", config_name="config", version_base="1.3")
def main(cfg: DictConfig) -> None:
    seed_everything(cfg.seed, workers=True)

    # ------------------------------------------------------------------
    # Dataset & out-of-time split
    # ------------------------------------------------------------------
    use_film        = cfg.model.global_encoder == "film"
    use_naive_static = cfg.model.static_encoder == "naive"
    use_naive_global = cfg.model.global_encoder == "naive"
    dataset = DroughtDataset(
        processed_dir=cfg.data.processed_dir,
        history_length=cfg.data.history_length,
        lead_time=cfg.data.lead_time,
        periods_forward=cfg.data.periods_forward,
        dynamic_vars=list(cfg.data.dynamic_vars),
        use_global_scalars=cfg.data.use_global_scalars or use_film or use_naive_global,
        med_sst_agg=cfg.data.med_sst_agg,
        static_injection="naive" if use_naive_static else "none",
        global_injection="naive" if use_naive_global else "none",
    )

    train_ds, val_ds, test_ds = dataset.split_by_years(
        val_from=cfg.data.val_from_year,
        test_from=cfg.data.test_from_year,
    )

    # Wrap subsets to carry auxiliary inputs required by optional model components.
    # Seasonal static encoder → needs target month per sample.
    # FiLM global encoder     → needs last-timestep global scalar per sample.
    seasonal = cfg.model.static_encoder == "seasonal"

    def _wrap(ds):  # type: ignore[return]
        if seasonal and use_film:
            return MonthScalarSubset(ds.dataset, ds.indices)   # type: ignore[attr-defined]
        if seasonal:
            return MonthAwareSubset(ds.dataset, ds.indices)    # type: ignore[attr-defined]
        if use_film:
            return ScalarSubset(ds.dataset, ds.indices)        # type: ignore[attr-defined]
        return ds

    train_ds = _wrap(train_ds)
    val_ds   = _wrap(val_ds)
    test_ds  = _wrap(test_ds)

    if use_film:
        extra_info = f" | FiLM scalars: {dataset.n_global}"
    elif dataset.n_naive_channels:
        extra_info = f" | naive extra channels: {dataset.n_naive_channels}"
    else:
        extra_info = ""
    print(
        f"Split  —  train: {len(train_ds)}  val: {len(val_ds)}  test: {len(test_ds)}  samples\n"
        f"Input channels: {dataset.n_input_channels}  "
        f"({dataset.n_dynamic_vars} vars × {cfg.data.history_length} months"
        + extra_info
        + f")  grid: {dataset.H}×{dataset.W}"
    )

    _loader_kw = dict(
        batch_size=cfg.data.batch_size,
        num_workers=cfg.data.num_workers,
        pin_memory=cfg.data.pin_memory,
        persistent_workers=cfg.data.num_workers > 0,
    )
    train_loader = DataLoader(train_ds, shuffle=True,  **_loader_kw)
    val_loader   = DataLoader(val_ds,   shuffle=False, **_loader_kw)
    test_loader  = DataLoader(test_ds,  shuffle=False, **_loader_kw)

    # ------------------------------------------------------------------
    # Model
    # ------------------------------------------------------------------
    model = RCNNModule(
        mode=cfg.model.mode,
        embedding_size=cfg.model.embedding_size,
        hidden_state_size=cfg.model.hidden_state_size,
        kernel_size=cfg.model.kernel_size,
        groups=cfg.model.groups,
        dilation=cfg.model.dilation,
        n_cells_hor=cfg.model.n_cells_hor,
        n_cells_ver=cfg.model.n_cells_ver,
        history_length=cfg.model.history_length,
        periods_forward=cfg.model.periods_forward,
        batch_size=cfg.model.batch_size,
        num_of_additional_features=len(dataset.dynamic_var_names),
        boundaries=list(cfg.model.boundaries),
        num_classes=cfg.model.num_classes,
        values_range=cfg.model.values_range,
        drought_threshold=cfg.model.drought_threshold,
        loss_fn=cfg.model.loss_fn,
        pinball_quantile=cfg.model.pinball_quantile,
        drought_weight=cfg.model.drought_weight,
        weighted_loss_mode=cfg.model.weighted_loss_mode,
        static_encoder=cfg.model.static_encoder,
        static_emb_size=cfg.model.static_emb_size,
        n_static_in=dataset.static_features.shape[0],
        global_encoder=cfg.model.global_encoder,
        n_film_scalars=dataset.n_global if use_film else 0,
        n_extra_channels=dataset.n_naive_channels,
        film_hidden_size=cfg.model.film_hidden_size,
        dropout=cfg.model.dropout,
        lr=cfg.model.lr,
        weight_decay=cfg.model.weight_decay,
        lr_scheduler_patience=cfg.model.lr_scheduler_patience,
    )

    # Use register_buffer (not plain assignment) so all tensors are saved
    # in the checkpoint and automatically moved to the right device.
    model.register_buffer("mask", dataset.mask)
    model.register_buffer("rlat", torch.tensor(dataset.rlat))
    model.register_buffer("rlon", torch.tensor(dataset.rlon))

    train_indices = train_ds.indices  # type: ignore[attr-defined]
    target_start  = train_indices[0]  + cfg.data.history_length
    target_end    = train_indices[-1] + cfg.data.history_length + 1
    model.register_buffer("global_avg", dataset.spei[target_start:target_end].mean(dim=0))

    # Per-cell linear trend: fit OLS on training targets, precompute test predictions.
    # Using absolute time index as the predictor so test extrapolation is anchored to
    # the same scale as training (no re-centering needed).
    _off = cfg.data.history_length + cfg.data.lead_time - 1
    _train_t = np.array([i + _off for i in train_indices], dtype=np.float32)
    _test_t  = np.array([i + _off for i in test_ds.indices],   dtype=np.float32)  # type: ignore[attr-defined]
    _A_tr = np.stack([_train_t, np.ones_like(_train_t)], axis=1)          # (N_tr, 2)
    _Y_tr = dataset.spei[torch.from_numpy(_train_t.astype(np.int64))].reshape(len(_train_t), -1).numpy()
    _C, _, _, _ = np.linalg.lstsq(_A_tr, _Y_tr, rcond=None)               # (2, H*W)
    _A_te = np.stack([_test_t, np.ones_like(_test_t)], axis=1)
    _trend_test = (_A_te @ _C).reshape(len(_test_t), dataset.H, dataset.W)
    model.register_buffer("trend_test", torch.tensor(_trend_test, dtype=torch.float32))

    if cfg.model.static_encoder in ("single", "seasonal"):
        model.register_buffer("static_map", dataset.static_features)

    if cfg.model.loss_fn == "cellwise_pinball":
        target_spei = dataset.spei[target_start:target_end]
        q_map = (
            (target_spei <= cfg.model.drought_threshold).float().mean(dim=0).clamp(min=0.01, max=0.5)
        )
        model.criterion.set_q_map(q_map, dataset.mask)

    # ------------------------------------------------------------------
    # Logger
    # ------------------------------------------------------------------
    logger = _build_logger(cfg)

    # ------------------------------------------------------------------
    # Trainer
    # ------------------------------------------------------------------
    ckpt_dir = Path(cfg.checkpoints_dir)
    ckpt_dir.mkdir(parents=True, exist_ok=True)

    callbacks = [
        ModelCheckpoint(
            dirpath=ckpt_dir,
            filename="best",
            monitor="val/loss",
            mode="min",
            save_top_k=1,
        ),
        EarlyStopping(
            monitor=cfg.trainer.early_stopping_monitor,
            patience=cfg.trainer.early_stopping_patience,
            min_delta=cfg.trainer.early_stopping_min_delta,
            mode="min",
        ),
        EpochTimer(),
    ]

    trainer = Trainer(
        max_epochs=cfg.trainer.max_epochs,
        callbacks=callbacks,
        logger=logger,
        log_every_n_steps=cfg.trainer.log_every_n_steps,
        deterministic=cfg.trainer.deterministic,
        accelerator=cfg.trainer.accelerator,
        devices=cfg.trainer.devices,
        precision=cfg.trainer.precision,
    )
    print(
        f"Trainer  —  accelerator={cfg.trainer.accelerator!r}"
        f"  devices={cfg.trainer.devices!r}"
        f"  precision={cfg.trainer.precision!r}"
        f"  deterministic={cfg.trainer.deterministic!r}"
    )

    tag = _model_tag(cfg)

    if cfg.eval_only:
        # ── Evaluation-only: skip training, load checkpoint, run test ──
        ckpt = cfg.eval_checkpoint
        if not ckpt:
            raise ValueError("eval_only=true requires eval_checkpoint to be set")
        print(f"\nEval-only mode — loading {ckpt}")
        trainer.test(model, test_loader, ckpt_path=ckpt)
    else:
        # ── Normal training ─────────────────────────────────────────────
        trainer.fit(model, train_loader, val_loader)

        best_src = Path(callbacks[0].best_model_path)
        best_dst = Path(cfg.saved_models_dir) / f"best_model_{tag}.ckpt"
        best_dst.parent.mkdir(parents=True, exist_ok=True)
        shutil.copy2(best_src, best_dst)
        best_src.unlink()  # remove the Lightning checkpoint; we keep our own copy
        print(f"Best model saved → {best_dst}")

        # Save the Hydra overrides so this run can be re-evaluated later
        _SKIP = {"run_name=", "logger.run_name=", "eval_only=", "eval_checkpoint="}
        raw_overrides = HydraConfig.get().overrides.task
        kept = [o for o in raw_overrides if not any(o.startswith(s) for s in _SKIP)]
        overrides_dst = best_dst.with_suffix(".overrides")
        overrides_dst.write_text("\n".join(kept))

        trainer.test(model, test_loader)

    # ── Copy figures to figures/models/{tag}/ ─────────────────────────
    if cfg.eval_only:
        fig_src = model._log_dir()
        fig_dst = Path(cfg.figures_dir) / tag
        fig_dst.mkdir(parents=True, exist_ok=True)
        copied = 0
        for fig in fig_src.glob("*.png"):
            shutil.copy2(fig, fig_dst / fig.name)
            copied += 1
        if copied:
            print(f"Figures ({copied}) → {fig_dst}")


if __name__ == "__main__":
    main()