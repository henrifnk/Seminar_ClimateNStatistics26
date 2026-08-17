The file CWG_NU_melt_jja_temp.txt contains annual reconstructions of
surface melt and summer temperature for the western Greenland Ice Sheet.
It was released as supplementary data with Boers and Rypdal (2021,
Proceedings of the National Academy of Sciences,
DOI: 10.1073/pnas.2024192118) and downloaded on 10 August 2026 from the
authors' public repository at https://github.com/niklasboers/GrIS-EWS.

The file has no header. Each row corresponds to one year, stored from
2013 back to 1650, with four space-separated columns: year; Central-
Western Greenland (CWG) June-July-August temperature in °C; a stacked 
CWG melt series in z-scores; and a separate NU ice-core melt series, 
also in z-scores. Temperature is missing (NaN) in many years before 
1855, and the CWG melt series begins only in 1675.

This chapter uses the CWG stacked melt series to reproduce the
statistical Early Warning Signal analysis of Boers and Rypdal (2021):
log-transformation, Gaussian detrending, and sliding-window variance
and lag-1 autocorrelation over the period 1855-2013. The temperature
and NU melt columns are kept with the original file but are not used
in the reproduction. The analysis code is in
work/09-tp-overview/R/ews_reproduction.R.
