stataR
======

A tentative package for Stata users built on `data.table`
for less intuitive commands. This is a beta version. Variable arguments accept character vectors and wildcards. They can be negated with "-"

````R
# edo allows to use some stata commands. Abbreviations are allowed.
DT %>% edo(order,"v*")
DT %>% edo(sort,"v*")
DT %>% edo(rename,"v1","v11")
DT %>% edo(keep,"v?")
DT %>% edo(keep,-"id*")
DT %>% edo(summarize,"v1")
DT %>% edo(sum,"v*",d=T)

## ereplace applies one function to several variables
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
DT %>% panel(id="id1 id2",t="time",L3.v2,gen="L3.v2")
````



Package can be installed via `devtools`

````R
library(devtools)
devtools::install_github("matthieugomez/stataR")
````
