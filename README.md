stataR
======

A tentative package for Stata users

````R
DT %>% edo(order,"v*")
DT %>% edo(sort,"v*")
DT %>% edo(rename,"v1","v11")
DT %>% edo(keep,"v?")

DT %>% edo(summarize,"v1")
DT %>% edo(sum,"v*",d=T)


## apply one function to several variables


DT %>% ereplace(mean,"v1","v1_mean",i=,by=,na.rm=T)
DT %>% ereplace(max,c("v1","v2"),by=year)
DT %>% ereplace(max,-c("id1","id2"),by=year)
DT %>% ereplace(max,-"id*",by=year)
DT %>% ereplace(max,"v*",by=year)
DT %>% ereplace(as.character,"*")






Variable arguments accept wildcards or character vectors and can be negated with "-"