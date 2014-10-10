statar
======

A set of R commands for Stata users built on data.table and dplyr.

The package can be installed via `devtools`

````R
devtools::install_github("matthieugomez/statar")
````

The package should be loaded after `dplyr`  and `lubridate` since it overwrites `dplyr::lag`, `dplyr::lead`, and `lubridate::floor_date`.
# vector functions
The package adds the following vector functions
````R
# lag/lead create lag/lead variables (corresponds to Stata L. F.)
year <- c(1992, 1989, 1991, 1990, 1994, 1992, 1991)
value <- c(4.1, 4.5, 3.3, 5.3, 3.0, 3.2, 5.2)
lag(value, 1, order_by = year) # returns value in previous year, like  dplyr::lag
lag(value, 1, along_with = year) #  returns value in year - 1

library(lubridate)
date <- mdy(c("03/01/1992", "04/03/1992", "07/15/1992", "08/21/1992"))
value <- c(4.1, 4.5, 3.3, 5.3)
datem <- floor_date(date, "month")
value_l <- lag(value, units = "month", along_with = datem) 

# tag (corresponds to Stata tag)
tag(c(1, 2))
tag(c(1, 2), fromLast = TRUE)

# sample_mode returns the statistical mode
sample_mode(c(1, 2, 2))
sample_mode(c(1, 2))
sample_mode(c(NA, NA, 1))
sample_mode(c(NA, NA, 1), na.rm = TRUE)

# partition creates integer variable for quantile categories (corresponds to Stata xtile)
v <- c(NA, 1:10)                   
partition(v, n_quantiles = 3) # 3 groups based on terciles
partition(v, probs = c(0.3, 0.7)) # 3 groups based on two quantiles
partition(v, cutpoints = c(2, 3)) # 3 groups based on two cutpoints

# winsorize (default based on 5 x interquartile range)
v <- c(1:4, 99)
winsorize(c(1:4, 99))
winsorize(c(1:4, 99), replace = NA)
winsorize(c(1:4, 99), probs = c(0.01, 0.99))
winsorize(c(1:4, 99), cutpoints = c(1, 50))
````

# data.table functions
The package adds the following verbs for data.tables.  Syntax for variable selections works similarly to `dplyr`.  

````R
library(data.table)

#setcols keeps certain columns
DT <- data.table(
  id = c(1,2),
  v1 = c(1,1),
  v2 = c(2,1)
)
setcols(DT, id, v2)
setcols(DT, -id)

# sum_up prints detailed summary statistics (corresponds to Stata summarize)
N <- 100; K <- 10
DT <- data.table(
  id = 1:N,
  v1 = sample(5, N, TRUE),
  v2 = sample(1e6, N, TRUE)
)
sum_up(DT)
sum_up(DT, v2, d = T)
sum_up(DT, starts_with("v"), by = v1)

# duplicates returns rows in duplicated groups
DT <- data.table(a = rep(1:2, each = 3), b = 1:6)
duplicates(DT, by = a)
duplicates(DT, by = list(a,b))

# fill_gap fills in gaps in a time variable (corresponds to Stata tsfill)
DT <- data.table(
    id    = c(1, 1, 1, 1, 1, 2, 2),
    year  = c(1992, 1989, 1991, 1990, 1994, 1992, 1991),
    value = c(4.1, 4.5, 3.3, 5.3, 3.0, 3.2, 5.2)
)
DT <- data.table(
    id    = c(1, 1, 1, 1, 1, 2, 2),
    year  = c(1992, 1989, 1991, 1990, 1994, 1992, 1991),
    value = c(4.1, 4.5, 3.3, 5.3, 3.0, 3.2, 5.2)
)
fill_gap(DT, value, along_with = year, by = id)
fill_gap(DT, value, along_with = year, by = id, full = TRUE)
library(lubridate)
DT[, date:= mdy(c("03/01/1992", "04/03/1992", "07/15/1992", "08/21/1992", "10/03/1992", "07/15/1992", "08/21/1992"))]
DT[, datem :=  floor_date(date, "month")]
fill_gap(DT, value, along_with = datem , units = "month", by = id)

# setna fills in missing values along a time variable
DT <- data.table(
 id    = c(1, 1, 1, 1, 1, 2, 2),
 date  = c(1992, 1989, 1991, 1993, 1994, 1992, 1991),
 value = c(NA, NA, 3, 5.3, NA, 3.2, 5.2)
)
DT1 <- copy(DT)
setkey(DT1, id, date)
DT2 <- copy(DT1)
DT3 <- copy(DT1)
setna(DT, value, along_with = date, by = id)
setna(DT1)
setna(DT2, value, rollends = TRUE)
setna(DT3, value, roll = "nearest")
````

Every function also has a version that accepts strings, formulas or quoted expressions : its name is the original function's name with the suffix "_" (see the [dplyr vignette](https://github.com/hadley/dplyr/blob/master/vignettes/nse.Rmd) for more details). For instance,

````R
N <- 100; K <- 10
DT <- data.table(
  id = 1:N,
  v1 = sample(5, N, TRUE),
  v2 = sample(1e6, N, TRUE)
)
sum_up(DT, id, v1)
sum_up(DT, list(id, v1))
sum_up_(DT, c("id", "v1")) 
sum_up_(DT, quote(list(id, v1))) 
````

# join
The package adds a wrapper for data.table merge functionalities.

- The option "type" specifies the type of join based on SQL syntax. Possible types are : left, right, inner, outer, semi, anti and cross.
- The option "check" checks there are no duplicates in the master or using data.tables (as in Stata).
- The option "gen" specifies the name of a new variable that identifies non matched and matched rows (as in Stata).

````R
library(data.table)
x <- data.table(a = rep(1:2, each = 3), b = 1:6)
y <- data.table(a = 0:1, bb = 10:11)
# outer corresponds to Stata joinby keep(master matched using)
join(x, y, type = "outer")
# left corresponds to Stata joinby keep(master matched)
join(x, y, type = "left")
# right corresponds to Stata joinby keep(mached using)
join(x, y, type = "right")
# inner corresponds to Stata joinby keep(matched)
join(x, y, type = "inner")

join(x, y, type = "semi")
join(x, y, type = "anti")
join(x, y, type = "outer", check = 1~m)
join(x, y, type = "outer", gen = "_merge")
````

# others
- A data.table method for the generic `tidyr::spread` that relies on `dcast.data.table` (much faster).
- `floor_date`, originally from the package `lubridate`, now accepts "quarter" as an argument 
- `tempname` creates a name not assigned in the environment specified by the second variable

	````R
	tempvar <- tempname("temp", DT)
	tempname <- tempname("temp", globalenv())
	````


