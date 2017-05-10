statar
======
[![Build Status](https://travis-ci.org/matthieugomez/statar.svg?branch=master)](https://travis-ci.org/matthieugomez/statar)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/statar)](https://cran.r-project.org/package=statar)
[![Coverage Status](https://img.shields.io/codecov/c/github/matthieugomez/statar/master.svg)](https://codecov.io/github/matthieugomez/statar?branch=master)


This package contains R functions corresponding to useful Stata commands.

The package includes:
- [data.frame functions](vignettes/data-frames.Rmd) (summarize, tabulate, merge)
- [panel data functions](vignettes/panel-data.Rmd) (monthly/quarterly dates, lead/lag, fillin)
- [vector functions](vignettes/vector.Rmd) (xtile, pctile, winsorize)
- [graph functions](vignettes/graph.Rmd) (binscatter)

Some examples:
```R
library(dplyr)
library(statar)

starwars %>% sum_up(height)
 
#Variable │      Obs  Missing     Mean   StdDev      Min      Max 
#─────────┼───────────────────────────────────────────────────────
#  height │       81        6  174.358  34.7704       66      264 

starwars %>% group_by(gender) %>% sum_up(height)

#>  gender │ Variable │      Obs  Missing     Mean   StdDev      Min      Max 
#>─────────┼──────────┼───────────────────────────────────────────────────────
#>  female │   height │       17        2  165.471  23.0302       96      213 
#>---------┼----------┼-------------------------------------------------------
#>hermap~e │   height │        1        0      175       NA      175      175 
#>---------┼----------┼-------------------------------------------------------
#>    male │   height │       59        3  179.237  35.3916       66      264 
#>---------┼----------┼-------------------------------------------------------
#>    none │   height │        1        1      200       NA      200      200 
#>---------┼----------┼-------------------------------------------------------
#>      NA │   height │        3        0      120  40.7063       96      167

starwars %>% tab(gender, eye_color)
 
#>       gender │     eye_color │    Freq.  Percent     Cum. 
#>──────────────┼───────────────┼────────────────────────────
#>       female │         black │        2     2.30     2.30 
#>       female │          blue │        6     6.90     9.20 
#>       female │         brown │        5     5.75    14.94 
#>       female │         hazel │        2     2.30    17.24 
#>       female │     red, blue │        1     1.15    18.39 
#>       female │       unknown │        1     1.15    19.54 
#>       female │         white │        1     1.15    20.69 
#>       female │        yellow │        1     1.15    21.84 
#>--------------┼---------------┼----------------------------
#>hermaphrodite │        orange │        1     1.15    22.99 
#>--------------┼---------------┼----------------------------
#>         male │         black │        7     8.05    31.03 
#>         male │          blue │       13    14.94    45.98 
#>         male │     blue-gray │        1     1.15    47.13 
#>         male │         brown │       16    18.39    65.52 
#>         male │          dark │        1     1.15    66.67 
#>         male │          gold │        1     1.15    67.82 
#>         male │ green, yellow │        1     1.15    68.97 
#>         male │         hazel │        1     1.15    70.11 
#>         male │        orange │        7     8.05    78.16 
#>         male │          pink │        1     1.15    79.31 
#>         male │           red │        2     2.30    81.61 
#>         male │       unknown │        2     2.30    83.91 
#>         male │        yellow │        9    10.34    94.25 
#>--------------┼---------------┼----------------------------
#>         none │         black │        1     1.15    95.40 
#>         none │           red │        1     1.15    96.55 
#>--------------┼---------------┼----------------------------
#>           NA │           red │        2     2.30    98.85 
#>           NA │        yellow │        1     1.15   100.00 
```


You can install 

- The latest released version from [CRAN](https://CRAN.R-project.org/package=statar) with

	```R
	install.packages("statar")
	```
-  The current version from [github](https://github.com/matthieugomez/statar) with

	```R
	devtools::install_github("matthieugomez/statar")
	```

