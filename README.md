statar
======

A set of R commands for data analysis built on data.table, dplyr and ggplot2.

- A  [vignette](vignettes/main.Rmd) presents the main functions of the package
- A companion guide to R for Stata users is [available on line](http://www.princeton.edu/~mattg/statar/)
- The CRAN version of this package is also [available](http://cran.r-project.org/web/packages/statar/index.html)
- The github version of this package can be installed via `devtools`
````R
devtools::install_github("matthieugomez/statar")
````

- The package should be loaded after `dplyr`  and `lubridate` since it overwrites `dplyr::lag`, `dplyr::lead`, and `lubridate::floor_date`.

