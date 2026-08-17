The Early Warning Signal reproduction in this chapter uses annual
reconstructions of surface melt and summer temperature for the western
Greenland Ice Sheet, released as supplementary data with Boers and Rypdal
(2021, Proceedings of the National Academy of Sciences,
DOI: 10.1073/pnas.2024192118). The original file is
CWG_NU_melt_jja_temp.txt in the authors' public repository
https://github.com/niklasboers/GrIS-EWS. It was consulted on 10 August
2026. The raw file is not stored in this book repository.

The file has no header. Each row corresponds to one year, stored from
2013 back to 1650, with four space-separated columns: year; Central-
Western Greenland (CWG) June-July-August temperature in °C; a stacked
CWG melt series in z-scores; and a separate NU ice-core melt series,
also in z-scores. Temperature is missing (NaN) in many years before
1855, and the CWG melt series begins only in 1675.

The reproduction reads the CWG stacked melt series directly from that
GitHub repository and applies log-transformation, Gaussian detrending,
and sliding-window variance and lag-1 autocorrelation over 1855-2013.
The temperature and NU melt columns are not used. Code:
work/09-tp-overview/R/tp_ews_reproduction.R.
