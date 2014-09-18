stataR
======

A tentative package for Stata users built on `data.table`
for less intuitive commands. This is a beta version. Variable arguments accept character vectors and wildcards. They can be negated with "-"

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



Package can be installed via `devtools`

````R
library(devtools)
devtools::install_github("matthieugomez/stataR")
````
