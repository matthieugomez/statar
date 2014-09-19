stataR
======

A tentative beta package for Stata users built on `data.table`. The goal is to code common Stata idioms in R when they're harder to do in `data.table`.


````R
# eset for commands that modify dataset in place. Abbreviations for command names are allowed.
DT %>% eset(order,"v*")
DT %>% eset(sort,"v*")
DT %>% eset(rename,"v1","v11")
DT %>% eset(keep,"v?")
DT %>% eset(keep,-"id*")


# edo for commands that don't modify dataset. Abbreviations for command names are allowed.
DT %>% edo(summarize,"v1")
DT %>% edo(sum,"v*",d=T)


## ereplace applies one function to several variables. Any functions that operate on vectors is allowed.
DT %>% ereplace(max,c("v1","v2"),by="year")
DT %>% ereplace(max,-c("id1","id2"),by="id*")
DT %>% ereplace(max,-"id*",by="year")
DT %>% ereplace(max,"v*",by="year")
DT %>% ereplace(as.character,"*")


## Panel
DT %<>% panel(id="id1 id2",t="time",fill)
#  %<>%, from Magrittr, is just equivalent to DT <- DT %>% panel(id="id1 id2",t="time",fill)
# Reassignment is needed rows cannot be dropped/created by reference in data.table (for now)
DT %>% panel(id="id1 id2",t="time",L3.v2)
DT %>% panel(id="id1 id2",t="time",L3.v2,gen="L3_v2")
````

## Join
join(DTm,DTu,1:1,all=T,gen="_merge")
join(DTm,DTu,m:1,all=T,gen="_merge")
join(DTm,DTu,1:1,all=T,nogen=T)
join(DTm,DTu,m:m,all.x=T)
join(DTm,DTu,m:m,all.y=T)
# m:m does a multiple to multiple matches, similar to Stata `joinby` and *not* to Stata `merge`. Default option are the ones specified in the first line. Merge is on variables with t he same name. Datasets are coerced to data.tables and eventually resorted.

Variable arguments accept character vectors and wildcards. They can be negated with "-"



Package can be installed via `devtools`

````R
library(devtools)
devtools::install_github("matthieugomez/stataR")
````
