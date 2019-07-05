statar
======
[![Build Status](https://travis-ci.org/matthieugomez/statar.svg?branch=master)](https://travis-ci.org/matthieugomez/statar)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/statar)](https://cran.r-project.org/package=statar)
[![Coverage Status](https://img.shields.io/codecov/c/github/matthieugomez/statar/master.svg)](https://codecov.io/github/matthieugomez/statar?branch=master)


This package contains R functions corresponding to useful Stata commands.

The package includes:
- [panel data functions](vignettes/panel-data.Rmd) (monthly/quarterly dates, lead/lag, fillin)
- [data.frame functions](vignettes/data-frames.Rmd) (tabulate, merge)
- [vector functions](vignettes/vector.Rmd) (xtile, pctile, winsorize)
- [graph functions](vignettes/graph.Rmd) (binscatter)



# Panel Data Functions

### Elapsed dates

The classes "monthly" and "quarterly"  print as dates and are compatible with usual time extraction (ie `month`, `year`, etc). Yet, they are stored as integers representing the number of elapsed periods since 1970/01/0 (resp in week, months, quarters). This is particularly handy for simple algebra:

```R
 # elapsed dates
 library(lubridate)
 date <- mdy(c("04/03/1992", "01/04/1992", "03/15/1992"))  
 datem <- as.monthly(date)
 # displays as a period
 datem
 #> [1] "1992m04" "1992m01" "1992m03"
 # behaves as an integer for numerical operations:
 datem + 1
 #> [1] "1992m05" "1992m02" "1992m04"
 # behaves as a date for period extractions:
 year(datem)
 #> [1] 1992 1992 1992
```


### lag / lead

`tlag`/`tlead` a vector with respect to a number of periods, **not** with respect to the number of rows

```R
year <- c(1989, 1991, 1992)
value <- c(4.1, 4.5, 3.3)
tlag(value, 1, time = year)
library(lubridate)
date <- mdy(c("01/04/1992", "03/15/1992", "04/03/1992"))
datem <- as.monthly(date)
value <- c(4.1, 4.5, 3.3)
tlag(value, time = datem) 
```


In constrast to comparable functions in `zoo` and `xts`, these functions can be applied to any vector and be used within  a `dplyr` chain:


```R
df <- tibble(
    id    = c(1, 1, 1, 2, 2),
    year  = c(1989, 1991, 1992, 1991, 1992),
    value = c(4.1, 4.5, 3.3, 3.2, 5.2)
)
df %>% group_by(id) %>% mutate(value_l = tlag(value, time = year))
```

### is.panel
`is.panel` checks whether a dataset is a panel i.e. the  time variable is never missing and the combinations (id, time) are unique.

```R
df <- tibble(
    id1    = c(1, 1, 1, 2, 2),
    id2   = 1:5,
    year  = c(1991, 1993, NA, 1992, 1992),
    value = c(4.1, 4.5, 3.3, 3.2, 5.2)
)
df %>% group_by(id1) %>% is.panel(year)
df1 <- df %>% filter(!is.na(year))
df1 %>% is.panel(year)
df1 %>% group_by(id1) %>% is.panel(year)
df1 %>% group_by(id1, id2) %>% is.panel(year)
```



### fill_gap
fill_gap transforms a unbalanced panel into a balanced panel.  It corresponds to the stata command `tsfill`. Missing observations are added as rows with missing values.
```R
df <- tibble(
    id    = c(1, 1, 1, 2),
    datem  = as.monthly(mdy(c("04/03/1992", "01/04/1992", "03/15/1992", "05/11/1992"))),
    value = c(4.1, 4.5, 3.3, 3.2)
)
df %>% group_by(id) %>% fill_gap(datem)
df %>% group_by(id) %>% fill_gap(datem, full = TRUE)
df %>% group_by(id) %>% fill_gap(datem, roll = "nearest")
```

# Data Frame Functions
### sum_up = summarize
`sum_up` prints detailed summary statistics (corresponds to Stata `summarize`)

```R
N <- 100
df <- tibble(
  id = 1:N,
  v1 = sample(5, N, TRUE),
  v2 = sample(1e6, N, TRUE)
)
sum_up(df)
df %>% sum_up(starts_with("v"), d = TRUE)
df %>% group_by(v1) %>%  sum_up()
```

### tab = tabulate
`tab` prints distinct rows with their count. Compared to the dplyr function `count`, this command adds frequency, percent, and cumulative percent.

```R
N <- 1e2 ; K = 10
df <- tibble(
  id = sample(c(NA,1:5), N/K, TRUE),
  v1 = sample(1:5, N/K, TRUE)       
)
tab(df, id)
tab(df, id, na.rm = TRUE)
tab(df, id, v1)
```



### join = merge
`join` is a wrapper for dplyr merge functionalities, with two added functions

- The option `check` checks there are no duplicates in the master or using data.tables (as in Stata).

  ```r
  # merge m:1 v1
  join(x, y, kind = "full", check = m~1) 
  ```
- The option `gen` specifies the name of a new variable that identifies non matched and matched rows (as in Stata).

  ```r
  # merge m:1 v1, gen(_merge) 
  join(x, y, kind = "full", gen = "_merge") 
  ```

- The option `update` allows to update missing values of the master dataset by the value in the using dataset


# Vector Functions

```R

# sample_mode returns the statistical mode
sample_mode(c(1, 2, 2))
sample_mode(c(1, 2))
sample_mode(c(NA, NA, 1))
sample_mode(c(NA, NA, 1), na.rm = TRUE)

# pctile computes quantile and weighted quantile of type 2 (similarly to Stata _pctile)
v <- c(NA, 1:10)                   
pctile(v, probs = c(0.3, 0.7), na.rm = TRUE) 

# xtile creates integer variable for quantile categories (corresponds to Stata xtile)
v <- c(NA, 1:10)                   
xtile(v, n_quantiles = 3) # 3 groups based on terciles
xtile(v, probs = c(0.3, 0.7)) # 3 groups based on two quantiles
xtile(v, cutpoints = c(2, 3)) # 3 groups based on two cutpoints

# winsorize (default based on 5 x interquartile range)
v <- c(1:4, 99)
winsorize(v)
winsorize(v, replace = NA)
winsorize(v, probs = c(0.01, 0.99))
winsorize(v, cutpoints = c(1, 50))
```






# Graph Functions
### stat_binmean

`stat_binmean()` is a `stat` for ggplot2. It returns the mean of `y` and `x` within bins of `x`. It's a bareborne version of the Stata command [binscatter](https://github.com/michaelstepner/binscatter)

```R
ggplot(iris, aes(x = Sepal.Width , y = Sepal.Length)) + stat_binmean()
ggplot(iris, aes(x = Sepal.Width , y = Sepal.Length, color = Species)) + stat_binmean(n=10) 
ggplot(iris, aes(x = Sepal.Width , y = Sepal.Length, color = Species)) + stat_binmean(n=10) + stat_smooth(method = "lm", se = FALSE)
```




# Installation
You can install 

- The latest released version from [CRAN](https://CRAN.R-project.org/package=statar) with

	```R
	install.packages("statar")
	```
-  The current version from [github](https://github.com/matthieugomez/statar) with

	```R
	devtools::install_github("matthieugomez/statar")
	```

