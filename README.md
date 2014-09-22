stataR
======

A tentative *beta* package for Stata users. I coded common Stata idioms when they're not intuitive in `data.table`.

Syntax is close to Stata:
- Arguments in the option `cols` accept character vectors and wildcards. They can be negated with "-". 
- Possible commands within `eset` and `edo` are predefined - which allows the use of shortcuts like `sum` instead of `summarize`.


````R
N=1e6; K=100
DT <- data.table(
  id = 1:N,
  v1 =  sample(5, N, TRUE),                          
  v2 =  sample(1e6, N, TRUE),                       
  v3 =  sample(round(runif(100,max=100),4), N, TRUE) 
)
# eset (modify dataset)
DT %>% eset(order,cols="v*")
DT %>% eset(sort,"v*")
DT %>% eset(rename,"v1","v11")
DT %>% eset(keep,-"id*")
DT %>% eset(keep,"v?")

# edo (don't modify dataset)
DT %>% edo(summarize,"v2")
DT %>% edo(sum,"v*",d=T)

## epanel 
DT <- data.table(
  id = c(1, 1, 1, 1, 1, 2, 2), 
  date = c(1992, 1989, 1991, 1990, 1994, 1992, 1991), 
  value = c(4.1, 4.5, 3.3, 5.3, 3.0, 3.2, 5.2)
)
DT %>% epanel(cols="id",t="date",L1.value)
DT %>% epanel(cols="id",t="date",L3.value,gen="L3.value")

DT <- DT %>% epanel(cols="id",t="date",fill)

## ejoin (creates new dataset)
ejoin(DTm,DTu,type=1:1,all=TRUE,gen="_merge")
ejoin(DTm,DTu,m:1)
ejoin(DTm,DTu,1:1,nogen=TRUE)
ejoin(DTm,DTu,m:m,all.x=TRUE)
ejoin(DTm,DTu,m:m,all.y=TRUE)

# datasets are coerced to data.tables and sorted in place.
# the command merges on common names between the two datasets and creates a new dataset.
# the option m:m creates multiple rows for multiple matches, similar to Stata joinby. 
# default options for all and gen are specified in the first line. 



## tempname(l) creates a name not present in the vector l
tempname <- tempname(names(DT))
````




The package can be installed via `devtools`

````R
library(devtools)
devtools::install_github("matthieugomez/stataR")
````
