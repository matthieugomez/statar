appliedr
======

A set of R commands for data analysis built on data.table, dplyr and ggplot2.

For a brief presentation of the package functionalities, please look at the [vignette](vignettes/main.Rmd)


- The github version of this package can be installed via `devtools`

````R
devtools::install_github("matthieugomez/appliedr")
````

- The package should be loaded after `dplyr`  and `lubridate` since it overwrites `dplyr::lag`, `dplyr::lead`, and `lubridate::floor_date`.


