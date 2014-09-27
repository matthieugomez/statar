statar
======

A tentative *beta* set of R commands for Stata users. 

Syntax is close to Stata:
- Arguments in the option `cols` can be character vectors and wildcards. They can be negated with "-". 
- Possible commands within `edo` are predefined - which allows the use of shortcuts like `sum` instead of `summarize`.


Examples:
````R
N=1e6; K=100
DT <- data.table(
  id = 1:N,
  v1 =  sample(5, N, TRUE),                          
  v2 =  sample(1e6, N, TRUE),                       
  v3 =  sample(round(runif(100, max = 100), 4), N, TRUE) 
)


# edo: stata commands that modify dataset
DT %>% eset(order, cols = "v*")
DT %>% eset(sort, c("v1", "v2"))
DT %>% eset(rename, "v1", "v11")
DT %>% eset(keep, -"id*")
DT %>% eset(keep, "v?")

# eset: stata commands that don't modify dataset
DT %>% edo(summarize, "v2")
DT %>% edo(sum, "v*", d = TRUE)

# epanel 
DT <- data.table(
  id = c(1, 1, 1, 1, 1, 2, 2), 
  date = c(1992, 1989, 1991, 1990, 1994, 1992, 1991), 
  value = c(4.1, 4.5, 3.3, 5.3, 3.0, 3.2, 5.2)
)
DT <- DT %>% epanel(cols = "id", t = "date", L1.value)
DT <- DT %>% epanel(cols = "id", t = "date", fill)

DT %>% epanel(cols = "id", t = "date", L3.value, gen = "L3.value", inplace = TRUE)

## ejoin 
ejoin(DTm, DTu, type = 1:1, keep = c("master", "matched", "using"), gen = "_merge")
ejoin(DTm, DTu, m:1, "matched")
ejoin(DTm, DTu, m:m, keep = c("master", "matched"), gen = FALSE)

# tempname (prefix,where) creates a name not assigned in the environment specified by the second variable
tempvar <- tempname("temp", DT)
tempname <- tempname("temp", globalenv())

````

The package can be installed via the package `devtools`

````R
devtools::install_github("matthieugomez/statar")
````
