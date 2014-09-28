statar
======

A tentative *beta* set of R commands for Stata users. 

Syntax is close to Stata:
- Arguments in the option `cols` can be character vectors and wildcards. They can be negated with "-". 
- Possible commands within `edo` are predefined - which allows the use of shortcuts like `sum` instead of `summarize`.


Examples:
````R
# eset: stata commands that modify dataset
N <- 100; K <- 10
DT <- data.table(
  id = 1:N,
  v1 =  sample(5, N, TRUE),
  v2 =  sample(1e6, N, TRUE),
  v3 =  sample(round(runif(100, max = 100), 4), N, TRUE)
)
DT %>% eset(order, cols = "v*")
DT %>% eset(sort, c("v1", "v2"))
DT %>% eset(rename, "v1", "v11")
DT %>% eset(keep, -"id*")
DT %>% eset(keep, "v?")



# Dplyr verbs (only work on data.tables)

### sum_up (= Stata summarize)
DT  %>% sum_up
DT  %>% sum_up(v3, d=T)
DT  %>% filter(v1==1) %>% sum_up(starts_with("v"))

### expand (= Stata tsfill)
DT <- data.table(
 id = c(1, 1, 1, 1, 1, 2, 2),
 date = c(1992, 1989, 1991, 1990, 1994, 1992, 1991),
 value = c(4.1, 4.5, 3.3, 5.3, 3.0, 3.2, 5.2)
)
DT <- DT %>% group_by(id) %>% expand(date)
DT <- DT %>% expand(date)


## Vector functions 
### quantile category (=Stata xtile)
DT %>% group_by(v1) %>% mutate(xtile(v2, nq = 3))
DT %>% group_by(v1) %>% mutate(xtile(v2, cutpoints = c(1e5,5e5) ))
### lag along (= Stata L. F.)
DT %>% group_by(id) %>% mutate(lag(value, order_by = time)) # Balanced dataset
DT %>% group_by(id) %>% mutate(lag(value, along = time)) # Unbalanced dataset


## ejoin 
ejoin(DTm, DTu, m:1)
ejoin(DTm, DTu, type = 1:1, keep = "matched", gen = "_merge")
ejoin(DTm, DTu, m:m, keep = c("master", "matched"), gen = FALSE)

## tidyr::spread is rewritten to use dcast.data.table when applied to data.tables, which makes it more memory efficient


# tempname creates a name not assigned in the environment specified by the second variable
tempvar <- tempname("temp", DT)
tempname <- tempname("temp", globalenv())
````

The package can be installed via the package `devtools`

````R
devtools::install_github("matthieugomez/statar")
````
