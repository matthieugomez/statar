stataR
======

A tentative package for Stata users

````R
DT %>% set(order,"v*")
DT %>% set(sort,"v*")
DT %>% set(sort,"v?")
DT %>% set(rename,"v1","v11")


DT %>% select(id) %>% describe
DT %>% select(id) %>% filter(v1==2) %>% describe(,d=T)



DT %>% egen(mean,"v1","v1_mean",by=year,filter=id==3)
DT %>% ereplace(max,"v*",by=year)
DT %>% ereplace(as.character,"*")
````
