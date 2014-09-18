stataR
======

A tentative package for Stata users

````R
DT %>% make(order,"v*")
DT %>% make(sort,"v*")
DT %>% make(rename,"v1","v11")
DT %>% make(keep,"v?")

DT %>% make(summarize,"v1")
DT %>% make(sum,"v*",d=T)


## apply a function to a list of variable

DT %>% egen(mean,"v1","v1_mean",i=,by=)
DT %>% egen(mean,"v1","v1_mean",i=,by=,na.rm=T)
DT %>% egen(mean,"v1 v2","v1_mean v2_mean",i=,by=)


DT %>% ereplace(max,"v*",by=year)
DT %>% ereplace(as.character,"*")


DT %>% edo(pmax,"v*",by=year)
````
