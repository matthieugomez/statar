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
value_l <- lag(value, months(1), along_with = datem) 

# tag (corresponds to Stata tag)
tag(c(1, 2))
tag(c(1, 2), fromLast = TRUE)

# sample_mode (corresponds to Stata mode)
sample_mode(c(1, 2, 2))
sample_mode(c(1, 2))
sample_mode(c(NA, NA, 1))
sample_mode(c(NA, NA, 1), na.rm = TRUE)

# partition creates integer variable for quantile categories (corresponds to Stata xtile)
v <- sample(c(NA, 1:10), 10, TRUE)                   
partition(v, nq = 3) # 3 groups based on terciles
partition(v, cutpoints = c(1, 3)) # 3 groups based on two cutpoints

# winsorize (default based on 5 x interquartile range)
winsorize(c(1, 2, 99))
winsorize(c(1, 2, 99), replace = NA)
winsorize(c(1, 2, 3, 99), cutpoints = quantile(c(1, 2, 3, 99), c(0.01, 0.99), type = 1))
````

# data.table functions
The package adds the following verbs for data.tables

````R
library(data.table)
library(dplyr)

# sum_up (= Stata summarize)
N=1e6; K=100
DT <- data.table(
  id = 1:N,
  v1 = sample(5, N, TRUE),
  v2 = sample(1e6, N, TRUE),
  )
DT %>% sum_up
DT %>% sum_up(v2, d=T)
DT %>% filter(v1==1) %>% sum_up(starts_with("v"))

# fill_gap (= Stata tsfill)
DT <- data.table(
    id    = c(1, 1, 1, 1, 1, 2, 2),
    date  = c(1992, 1989, 1991, 1990, 1994, 1992, 1991),
    value = c(4.1, 4.5, 3.3, 5.3, 3.0, 3.2, 5.2)
)
DT %>% group_by(id) %>% fill_gap(value, along_with = date)
DT %>% group_by(id) %>% fill_gap(value, along_with = date, type = "across")

# fill na (in a new dataset)
DT <- data.table(
 id    = c(1, 1, 1, 1, 1, 2, 2),
 date  = c(1992, 1989, 1991, 1990, 1994, 1992, 1991), 
 value = c(NA, NA, 3, 5.3, 3.0, 3.2, 5.2)
)
DT %>% group_by(id) %>% fill_na(value, along_with = date) 
DT %>% group_by(id) %>% fill_na(value, along_with = date, roll = "nearest")

# fill na (in the original dataset)
DT <- data.table(
 id    = c(1, 1, 1, 1, 1, 2, 2),
 date  = c(1992, 1989, 1991, 1990, 1994, 1992, 1991), 
 value = c(NA, NA, 3, 5.3, 3.0, 3.2, 5.2)
)
setkeyv(DT,c("id", "date"))
setna(DT, "value")
setna(DT, "value", roll = "nearest")
````


# join
The package adds a wrapper for the data.table merge command.

- The option "type" is based on SQL syntax. Possible types are : left, right, inner, outer, semi and anti. 
- The option "check" checks there are no duplicates in the master or using data.tables (as in Stata)
- The option "gen" specifies the name of a new variable that identifies non matched and matched rows (as in Stata)

````R
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


