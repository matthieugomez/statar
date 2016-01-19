# News


## 0.3

- `stat_binmean` allows to plot the mean of y over the mean of x in given xtiles
- new function `pctile`  corresponds to Stata _pctile (quantile type 2 weighted)

## 0.2
- `lag(, along_with)` and `lead(, along_with)` become separate functions with prefix t: `tlag(, along_with)` and `tlead(, along_with)`. The usual lag/lead based on rows can still be found in dplyr
- `bin` is renamed to `xtile`
- In join, the option type = "outer" is renamed to type = "full" similarly to dplyr
- `sum_up`, `tab`, and `find_duplicates` now accept data.frames and grouped dataframes
