appliedr
======

A set of R commands for data analysis built on data.table, dplyr and ggplot2.

The package can be installed via `devtools`

````R
devtools::install_github("matthieugomez/appliedr")
````

The package should be loaded after `dplyr`  and `lubridate` since it overwrites `dplyr::lag`, `dplyr::lead`, and `lubridate::floor_date`.

For a brief presentation of the package, please look at the [vignette](vignettes/main.Rmd)

