# Data — Chapter 03: Hydrology under Climate Change

## Included in this repository

- `gauge_regions_official.csv` — assignment of each CAMELS-CH catchment to one of the
  seven FOEN bio-geographic regions (Jura, Black Forest, Plateau, Alps North/West/South/East),
  computed by spatial overlay of the official FOEN region polygons with the CAMELS-CH
  catchment polygons (largest-area-share rule; see `code/assign_regions_official.py`).
- `gauge_regions.csv` — earlier approximate assignment via a lon/lat/elevation point rule,
  kept for comparison (agrees with the official overlay for only ~49 % of catchments).

## Not included (too large for git): CAMELS-CH raw data

The analysis reads daily observed specific discharge (`discharge_spec`, mm/d) from
CAMELS-CH (Höge et al., 2023). Download it from Zenodo:

- https://doi.org/10.5281/zenodo.7784632

and place the time series under:

```
data/camels_ch/timeseries/observation_based/CAMELS_CH_obs_based_<gauge_id>.csv
```

(relative to the folder from which the scripts in `../code/` are run).

The FOEN bio-geographic region shapefile (`N2020_Revision_BiogeoRegion.shp`, EPSG:2056)
used by `assign_regions_official.py` is available from the Swiss Federal Office for the
Environment (FOEN/BAFU) open data portal ("Biogeographische Regionen der Schweiz").
