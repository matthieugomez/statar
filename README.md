statar
======

statar makes it easier to work with tabular datasets in R if you're used to Stata. It contains the following functions:
- summary commands such as `summarize` and `tab`.
- vector functions `xtile`, `pctile`. 
- join datasets with `1:m/m:1` check and a `_merge` variable,
- panel data commands such as `tsset`, `tsfill`, `lag`, `duplicates`
- a class for monthly and quarterly dates.
- a stat for `ggplot` that reproduces the Stata commmand `binscatter`

Vignettes present [data.frame functions](vignettes/data.frame.Rmd), [panel data functions](vignettes/panel-data.Rmd), [graph functions](vignettes/graph.Rmd) , and [vector functions](vignettes/vector.Rmd).

You can install 

- The latest released version from [CRAN](http://cran.r-project.org/web/packages/statar/index.html) with

	```R
	install.packages("statar")
	```
-  The current version from [github](https://github.com/matthieugomez/statar) with

	```R
	devtools::install_github("matthieugomez/statar")
	```

