stataR
======

A tentative *beta* package for Stata users. The goal is to code common Stata idioms in R when they're harder to do in `data.table`.

Variable arguments accept character vectors and wildcards. They can be negated with "-". 
Possible commands within `eset` and `edo` are predefined - allowing for the use of shortcuts.



````R
N=1e6; K=100
DT <- data.table(
  id = 1:N,
  v1 =  sample(5, N, TRUE),                          # int in range [1,5]
  v2 =  sample(1e6, N, TRUE),                        # int in range [1,1e6]
  v3 =  sample(round(runif(100,max=100),4), N, TRUE) # numeric e.g. 23.5749
)
# eset (inplace). Abbreviations for command names are allowed.
DT %>% eset(order,"v*")
DT %>% eset(sort,"v*")
DT %>% eset(rename,"v1","v11")
DT %>% eset(keep,-"id*")
DT %>% eset(keep,"v?")

# edo for commands that don't modify dataset. Abbreviations for command names are allowed.
DT %>% edo(summarize,"v2")
DT %>% edo(sum,"v*",d=T)

## Panel (inplace)
DT <- data.table(
  id = c(1, 1, 1, 1, 1, 2, 2), 
  date = c(1992, 1989, 1991, 1990, 1994, 1992, 1991), 
  value = c(4.1, 4.5, 3.3, 5.3, 3.0, 3.2, 5.2)
)
DT %>% epanel(id="id",t="date",L1.value)
DT %>% epanel(id="id",t="date",L3.value,gen="L3.value")

DT <- DT %>% epanel(id="id",t="date",fill)

## Join (creates new dataset)
join(DTm,DTu,type=1:1,all=TRUE,gen="_merge")
join(DTm,DTu,m:1,all=TRUE,gen="_merge")
join(DTm,DTu,1:1,all=TRUE,nogen=TRYE)
join(DTm,DTu,m:m,all.x=TRUE)
join(DTm,DTu,m:m,all.y=TRUE)

# m:m does a multiple to multiple matches, similar to Stata `joinby` and *not* to Stata `merge`. 
#Default option for all and gen are the ones specified in the first line. 
# The merge is based on common names between the two datasets. 
# Datasets are eventually coerced to data.tables and  sorted in place.
# The command join creates a new dataset.


## newname
newname <- tempname(names(DT))
````




The package can be installed via `devtools`

````R
library(devtools)
devtools::install_github("matthieugomez/stataR")
````
