import json
from pathlib import Path

import numpy as np
import torch
import xarray as xr
from torch.utils.data import Dataset, Subset
from utils.paths import validate_processed_dir

# Western/Eastern Mediterranean split along the Sicily Channel (Millot &
# Taupier-Letage 2005; Lionello et al. 2006).  Mediterranean_Sea and Western_Med
# are pre-computed aggregates already in the source file and are excluded here
# to avoid double-counting when averaging within groups.
_MED_GROUPS: dict[str, list[str]] = {
    "western": ["Alboran_Sea", "Balearic_Sea", "Ligurian_Sea", "Gulf_of_Lion",
                "Tyrrhenian_Sea", "Sicilian_Sea"],
    "eastern": ["Adriatic_Sea", "Ionian_Sea", "Lybian_Sea", "Aegean_Sea", "Levantine_Basin"],
    "black_sea": ["Black_Sea"],
}


class DroughtDataset(Dataset):
    """
    Sliding-window dataset over the preprocessed AL domain.

    Feature layout per sample
    ─────────────────────────
    x  shape: (history_length, n_dynamic_vars, H, W)

      Sequence of frames, one per input month, oldest-first.
      Within each frame the channel order is:
        [spei_t, wb_t, pr_t, ps_t, tas_t, …]
      SPEI is always channel 0, so the last observed SPEI
      is x[-1, 0].

    Global scalars (NAO index, Mediterranean SST, …) are loaded into
    self.global_scalars (T, n_global) when use_global_scalars=True but are
    NOT part of x.  They are consumed exclusively by the FiLM generator
    (global_encoder='film') via ScalarSubset / MonthScalarSubset wrappers.

    Static spatial features (orogf + mask) are always loaded into
    self.static_features (2, H, W) but are NOT part of x — they are
    consumed exclusively by the model's static encoder (single or seasonal).

    y  shape: (periods_forward, H, W)
        SPEI for the next periods_forward months.

    mask  shape: (H, W) bool
        True where Alpine data exists. Used by the model to restrict
        loss and metric computation to valid cells.
    """

    def __init__(
        self,
        processed_dir: Path | str,
        history_length: int = 12,
        lead_time: int = 12,
        periods_forward: int = 1,
        dynamic_vars: list[str] | None = None,
        use_global_scalars: bool = False,
        med_sst_agg: str = "grouped",
        static_injection: str = "none",   # none | naive
        global_injection: str = "none",   # none | naive
    ):
        processed_dir = validate_processed_dir(processed_dir)

        # ── normalization stats (computed from training period only) ─────
        norm_path = processed_dir / "normalization.json"
        with open(norm_path) as f:
            _norm: dict[str, dict[str, float]] = json.load(f)

        # Load mask early so non-Alpine cells can be filled with the variable
        # mean *before* normalizing. Without this, cells filled with 0 become
        # extreme outliers after normalization (e.g. ps=0 Pa → −13 std devs).
        _mask_np = xr.open_dataset(processed_dir / "static_grid.nc")["mask"].values.astype(bool)

        # ── dynamic grid ──────────────────────────────────────────────
        ds_dyn = xr.open_dataset(processed_dir / "dynamic_grid.nc")

        def _load_dyn(var: str) -> torch.Tensor:
            arr  = ds_dyn[var].transpose("time", "rlat", "rlon").values.copy()
            mean = _norm[var]["mean"]
            std  = max(_norm[var]["std"], 1e-8)
            arr[:, ~_mask_np] = mean   # fill non-Alpine cells with mean → 0 after norm
            return torch.tensor((arr - mean) / std, dtype=torch.float32)

        self.spei = _load_dyn("spei")                          # (T, H, W)

        _extra_vars = dynamic_vars if dynamic_vars is not None else ["wb", "pr", "ps", "tas"]
        self.dynamic_others = torch.stack(
            [_load_dyn(v) for v in _extra_vars], dim=1
        ) if _extra_vars else None                              # (T, n_dyn, H, W) or None

        self.T, self.H, self.W = self.spei.shape
        self.rlat: np.ndarray = ds_dyn["rlat"].values.astype(np.float32)
        self.rlon: np.ndarray = ds_dyn["rlon"].values.astype(np.float32)
        self.history_length = history_length
        self.lead_time = lead_time          # gap between last input and first target
        self.periods_forward = periods_forward
        self.dynamic_var_names = _extra_vars
        self.static_injection = static_injection
        self.global_injection = global_injection

        # target_times[i] = date of the prediction target for sample i
        # = first input month + history_length + lead_time - 1
        times = ds_dyn["time"].values
        n_samples = self.T - history_length - lead_time - periods_forward + 2
        t0 = history_length + lead_time - 1
        self.target_times: np.ndarray = times[t0: t0 + n_samples]
        ds_dyn.close()

        # ── valid-cell mask ───────────────────────────────────────────
        ds_static = xr.open_dataset(processed_dir / "static_grid.nc")
        self.mask: torch.Tensor = torch.tensor(
            ds_static["mask"].values.astype(bool), dtype=torch.bool
        )

        # ── static spatial features ───────────────────────────────────
        # Always loaded; consumed by the static encoder in the model, never in x.
        orogf = torch.tensor(ds_static["orogf"].values, dtype=torch.float32)
        self.static_features: torch.Tensor = torch.stack([orogf, self.mask.float()], dim=0)
        ds_static.close()

        # ── global scalar features (optional) ────────────────────────
        self.global_scalars: torch.Tensor | None = None
        self.n_global: int = 0
        if use_global_scalars:
            ds_glob = xr.open_dataset(processed_dir / "global_scalars.nc")
            glob_norm: dict = _norm.get("_global", {})

            _CYCLICAL = {"month_sin", "month_cos"}

            def _load_scalar(var: str) -> torch.Tensor:
                arr = ds_glob[var].values.astype(np.float32)
                # Cyclical encodings are already in [-1, 1]; skip normalization
                # to preserve their geometric meaning across uneven month distributions.
                if var in glob_norm and var not in _CYCLICAL:
                    arr = (arr - glob_norm[var]["mean"]) / glob_norm[var]["std"]
                return torch.tensor(arr, dtype=torch.float32)

            # Always include NAO and cyclical month encoding
            parts: list[torch.Tensor] = [
                _load_scalar("nao_index"),
                _load_scalar("month_sin"),
                _load_scalar("month_cos"),
            ]

            if med_sst_agg == "grouped":
                # Regional averages over normalised individual basins
                # (W/E split at Sicily Channel; Black Sea separate)
                for basins in _MED_GROUPS.values():
                    group = torch.stack([_load_scalar(b) for b in basins], dim=1).mean(dim=1)
                    parts.append(group)
            else:
                # All 14 basin variables as-is (including pre-computed aggregates)
                sst_vars = [v for v in ds_glob.data_vars
                            if v not in ("nao_index", "month_sin", "month_cos")]
                parts.extend(_load_scalar(v) for v in sst_vars)

            self.global_scalars = torch.stack(parts, dim=1)  # (T, n_global)
            self.n_global = self.global_scalars.shape[1]
            ds_glob.close()

    # ── channel counts ────────────────────────────────────────────────

    @property
    def n_dynamic_vars(self) -> int:
        """Total dynamic input variables (SPEI + additional)."""
        return 1 + len(self.dynamic_var_names)

    @property
    def n_naive_channels(self) -> int:
        """Extra channels appended to x under naive injection modes."""
        n = 0
        if self.static_injection == "naive":
            n += self.static_features.shape[0]   # 2 (orogf + mask)
        if self.global_injection == "naive":
            n += self.n_global
        return n

    @property
    def n_input_channels(self) -> int:
        """Channels per timestep in x (n_dynamic_vars + naive-injected channels per frame).

        In standard mode, global scalars and static features are NOT part of x.
        In naive injection mode, static features (2 channels) and/or global
        scalars (n_global channels) are appended as extra channels per frame.
        """
        return self.n_dynamic_vars + self.n_naive_channels

    # ── dataset protocol ──────────────────────────────────────────────

    def __len__(self) -> int:
        return self.T - self.history_length - self.lead_time - self.periods_forward + 2

    def __getitem__(self, idx: int) -> tuple[torch.Tensor, torch.Tensor]:
        sl = slice(idx, idx + self.history_length)

        # Dynamic: SPEI as channel 0 each frame → (h, 1, H, W)
        spei_hist = self.spei[sl].unsqueeze(1)

        if self.dynamic_others is not None:
            others = self.dynamic_others[sl]                    # (h, n_dyn, H, W)
            x = torch.cat([spei_hist, others], dim=1)          # (h, n_vars, H, W)
        else:
            x = spei_hist                                       # (h, 1, H, W)

        # Naive injection: static features / global scalars as extra channels per frame
        if self.static_injection == "naive":
            # (2, H, W) → (h, 2, H, W) — same spatial map repeated at every timestep
            static_exp = self.static_features.unsqueeze(0).expand(self.history_length, -1, -1, -1)
            x = torch.cat([x, static_exp], dim=1)

        if self.global_injection == "naive":
            assert self.global_scalars is not None, (
                "global_injection='naive' requires use_global_scalars=True"
            )
            # Full window aligned to input frames: (h, n_global) → (h, n_global, H, W)
            scalars_win = self.global_scalars[idx : idx + self.history_length]
            scalar_channels = scalars_win[:, :, None, None].expand(-1, -1, self.H, self.W)
            x = torch.cat([x, scalar_channels], dim=1)

        # Target: SPEI at exactly lead_time months after the last input step
        t_start = idx + self.history_length + self.lead_time - 1
        y = self.spei[t_start: t_start + self.periods_forward]  # (pf, H, W)

        return x, y

    # ── out-of-time split ─────────────────────────────────────────────

    def split_by_years(
        self,
        val_from: int,
        test_from: int,
    ) -> tuple[Subset, Subset, Subset]:
        """
        Out-of-time split anchored to the target year.

        Args:
            val_from:  First year of the validation period  (e.g. 2006).
            test_from: First year of the test period        (e.g. 2015).
        """
        years = self.target_times.astype("datetime64[Y]").astype(int) + 1970
        train_idx = np.where(years < val_from)[0].tolist()
        val_idx   = np.where((years >= val_from) & (years < test_from))[0].tolist()
        test_idx  = np.where(years >= test_from)[0].tolist()
        return Subset(self, train_idx), Subset(self, val_idx), Subset(self, test_idx)


class MonthAwareSubset(Subset):
    """Wraps a DroughtDataset Subset to also return the target month (1–12).

    Used by the seasonal static encoder so each batch element carries the
    month of its prediction target.  Returns (x, y, month) instead of (x, y).
    """

    def __getitem__(self, idx: int) -> tuple[torch.Tensor, torch.Tensor, int]:
        x, y = super().__getitem__(idx)
        sample_idx: int = self.indices[idx]  # index into the full DroughtDataset
        month = int(
            self.dataset.target_times[sample_idx].astype("datetime64[M]").astype("int64") % 12 + 1
        )
        return x, y, month

    def __getitems__(self, indices: list[int]) -> list:
        return [self.__getitem__(idx) for idx in indices]


class ScalarSubset(Subset):
    """Wraps a DroughtDataset Subset to also return the per-step global scalars
    over the input window.

    Used by the FiLM generator so each batch element carries the global climate
    indices (NAO, Med SST, …) for every input month as the conditioning signal.
    Returns (x, y, scalars) where scalars has shape (history_length, n_global).
    """

    def __getitem__(self, idx: int) -> tuple[torch.Tensor, torch.Tensor, torch.Tensor]:
        x, y = super().__getitem__(idx)
        sample_idx: int = self.indices[idx]
        scalars: torch.Tensor = self.dataset.global_scalars[
            sample_idx : sample_idx + self.dataset.history_length
        ]  # (h, n_global)
        return x, y, scalars

    def __getitems__(self, indices: list[int]) -> list:
        return [self.__getitem__(idx) for idx in indices]


class MonthScalarSubset(Subset):
    """Combined wrapper returning (x, y, month, scalars) for models using both
    a seasonal static encoder and FiLM conditioning simultaneously.
    """

    def __getitem__(
        self, idx: int
    ) -> tuple[torch.Tensor, torch.Tensor, int, torch.Tensor]:
        x, y = super().__getitem__(idx)
        sample_idx: int = self.indices[idx]
        month = int(
            self.dataset.target_times[sample_idx].astype("datetime64[M]").astype("int64") % 12 + 1
        )
        scalars: torch.Tensor = self.dataset.global_scalars[
            sample_idx : sample_idx + self.dataset.history_length
        ]  # (h, n_global)
        return x, y, month, scalars

    def __getitems__(self, indices: list[int]) -> list:
        return [self.__getitem__(idx) for idx in indices]