# Data for Chapter 07 (Change-Point Detection)

External climate data are **not** stored in this repository.
Download them locally before running the analysis notebook
(`work/07-change_stats/code/cc_stats_latest.ipynb`).

## Required file

| Local filename | Description |
|---|---|
| `berkeley_earth.txt` | Berkeley Earth **Global Land Only** monthly average temperature file (anomalies and related columns; whitespace-separated text with `%` comment header). |

### Where to get it

1. Open [Berkeley Earth Global Temperature Data](https://berkeleyearth.org/data/).
2. Under **Global Time Series Data**, choose **Global Land Only (1750 – Recent)**.
3. Download the land monthly average temperature series, or use the direct file:

   https://berkeley-earth-temperature.s3.us-west-1.amazonaws.com/Global/Complete_TAVG_complete.txt

4. Save the downloaded file in this folder as:

   `work/07-change_stats/data/berkeley_earth.txt`

### What the file contains

Monthly global land temperature estimates from 1750 onward, including:

- year and month,
- monthly temperature anomaly (relative to the 1951–1980 climatology),
- uncertainty columns,
- annual and multi-year smoothed series (not required for the Wavelet–CNN pipeline).

The notebook uses the **monthly anomaly** column as the univariate input series for change-point detection.

### Optional derived file

The notebook can write `BerkeleyEarth_land_absolute.csv` (absolute temperatures obtained by adding a fixed monthly climatology to the anomalies). That file is a **local by-product** and should not be committed to git.
