statar
======

A tentative *beta* set of R commands for Stata users. 

Syntax is close to Stata:
- Arguments in the option `cols` can be character vectors and wildcards. They can be negated with "-". 
- Possible commands within `edo` are predefined - which allows the use of shortcuts like `sum` instead of `summarize`.


Examples:
````R
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
DT <- DT %>% epanel(cols = "id", t = "date", L1.value)
DT <- DT %>% epanel(cols = "id", t = "date", fill)
DT %>% epanel(cols = "id", t = "date", L3.value, gen = "L3.value", inplace = TRUE)

## ejoin 
ejoin(DTm, DTu, m:1)
ejoin(DTm, DTu, type = 1:1, keep = "matched", gen = "_merge")
ejoin(DTm, DTu, m:m, keep = c("master", "matched"), gen = FALSE)

# tempname creates a name not assigned in the environment specified by the second variable
tempvar <- tempname("temp", DT)
tempname <- tempname("temp", globalenv())
````

The package can be installed via the package `devtools`

````R
devtools::install_github("matthieugomez/statar")
````
