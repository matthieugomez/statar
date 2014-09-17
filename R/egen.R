# DT %>% egen(mean,"v1","v1_mean")

egen=function(DT,fun,cols,gen,filter=TRUE,by=NULL,...){
  func=as.character(substitute(fun))
  colsc=unlist(str_split(cols,pattern=" "))
  cc=NULL
  for (c in colsc){
    cc=c(cc,names(DT)[which(str_detect(names(DT),glob2rx(c)))])
  }
 colsc=cc
 stopifnot(length(colsc)==length(gen) & !is.element(gen,names(DT)))
 eval(substitute(DT[filter,gen:=lapply(.SD,function(x){fun(x,...)}),by,.SD=colsc]))
}

