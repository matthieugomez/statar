statar
======

This package contains R functions corresponding to useful Stata commands.

The package includes:
- [vector functions](vignettes/vector.Rmd) (count, sample_mode, xtile, pctile, winsorize)
- [data.frame functions](vignettes/data-frames.Rmd) (summarize, tabulate, join)
- [panel data functions](vignettes/panel-data.Rmd) (elapsed dates, time lead/lag, is.panel, fill_gap)
- [graph functions](vignettes/graph.Rmd) (binscatter)

You can install 

- The latest released version from [CRAN](https://CRAN.R-project.org/package=statar) with

	```R
	install.packages("statar")
	```
-  The current version from [github](https://github.com/matthieugomez/statar) with

	```R
	devtools::install_github("matthieugomez/statar")
	```

