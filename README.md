statar
======

statar contains functions inspired by useful Stata commands.

The package includes:
- [vector functions](vignettes/vector.Rmd) (count, sample_mode, xtile, pctile, winsorize)
- [data.frame functions](vignettes/data-frames.Rmd) (summarize, tabulate, join)
- [panel data functions](vignettes/panel-data.Rmd) (elapsed dates, time lead/lag, is.panel, fill_gap)
- [graph functions](vignettes/graph.Rmd) (binscatter)

You can install 

- The latest released version from [CRAN](http://cran.r-project.org/web/packages/statar/index.html) with

	```R
	install.packages("statar")
	```
-  The current version from [github](https://github.com/matthieugomez/statar) with

	```R
	devtools::install_github("matthieugomez/statar")
	```

