statar
======

A set of R commands for Stata users built on dplyr and data.table. 


# vector functions
The package adds the following vector functions
````R
library(dplyr)
library(data.table)
library(statar)

# partition creates quantile categories (corresponds to Stata xtile)
v2 <- sample(1e6, 1e6, TRUE)                   
v2_categorized <- partition(v2, nq = 3) # 3 groups based on terciles
v2_categorized <- partition(v2, cutpoints = c(1e5, 5e5)) # 3 groups based on two cutpoints

# winsorize (default based on 5 x interquartile range)
winsorize(v)
winsorize(v, replace = NA)
winsorize(v, cutpoints = quantile(v, c(0.01, 0.99), na.rm = TRUE))

# lag/lead create lag/lead variables (corresponds to Stata L. F.)
## lag in unbalanced panel
year <- c(1992, 1989, 1991, 1990, 1994, 1992, 1991)
value <- c(4.1, 4.5, 3.3, 5.3, 3.0, 3.2, 5.2)
lag(value, 1, order_by = year) # returns value in previous year, like  dplyr::lag
lag(value, 1, along_with = year) #  returns value in year - 1
## lag by time periods
library(lubridate)
DT <- data.table(     
   id    = c("id1", "id1", "id1", "id1"),
   date  = mdy(c("03/01/1992", "04/03/1992", "07/15/1992", "08/21/1992")),
   value = c(4.1, 4.5, 3.3, 5.3)
)
DT[, datem := floor_date(date, "month")]
DT[, datem_l := lag(value, months(1), along_with = date), by = id] 
````

# data.table verbs

````R
# setna fills in missing values
DT <- data.table(
  id    = c(1, 1, 1, 1, 1, 2, 2),
  date  = c(1992, 1989, 1991, 1990, 1994, 1992, 1991),
  value = c(4.1, NA, NA, 5.3, 3.0, 3.2, 5.2)
 )
setkey(DT, id, date) 
setna(DT, "value", roll = 3)
````
# dplyr verbs

The package adds the following verbs built on dplyr syntax for data.tables

````R
N=1e6; K=100
DT <- data.table(
  id = 1:N,
  v1 = sample(5, N, TRUE),
  v2 = sample(1e6, N, TRUE),
  v3 = sample(round(runif(100, max = 100), 4), N, TRUE)
  )

# colorder (= Stata order)
DT  %>% colorder(starts_with("v"))
DT  %>% colorder(starts_with("v"), inplace = TRUE)

# sum_up (= Stata summarize)
DT  %>% sum_up
DT  %>% sum_up(v3, d=T)
DT  %>% filter(v1==1) %>% sum_up(starts_with("v"))

# expand (= Stata tsfill)
DT <- data.table(
    id    = c(1, 1, 1, 1, 1, 2, 2),
    date  = c(1992, 1989, 1991, 1990, 1994, 1992, 1991),
    value = c(4.1, 4.5, 3.3, 5.3, 3.0, 3.2, 5.2)
)
DT %>% expand(date)
DT %>% group_by(id) %>% expand(date, type = "within")
DT %>% group_by(id) %>% expand(date, type = "across")
````


# merge
The package adds a wrapper for `data.table::merge` based on Stata syntax

````R
# inner join
ejoin(DTm, DTu, keep = "matched")
# left join
ejoin(DTm, DTu, keep = c("master","matched"))
# full outer join
ejoin(DTm, DTu, keep = c("master","matched","using"))
# type and gen options
ejoin(DTm, DTu, type = "1:1", keep = c("master","matched","using"), gen = "_merge")
````

# other functions
-  data.table method for the generic `tidyr::spread` that relies on `dcast.data.table` (much faster).
- `floor_date`, originally from the package `lubridate`, now accepts "quarter" as an argument 
- `tempname` creates a name not assigned in the environment specified by the second variable

	````R
	tempvar <- tempname("temp", DT)
	tempname <- tempname("temp", globalenv())
	````

The package can be installed via the package `devtools`

````R
devtools::install_github("hadley/tidyr")
devtools::install_github("matthieugomez/statar")
````
The package should be loaded after `dplyr` (>= v0.3) and `lubridate`.
