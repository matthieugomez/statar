# DT %>% ereplace(as.character,"v1*")


ereplace=function(DT,fun,cols=names(DT),filter=TRUE,by=NULL,...){
  func=as.character(substitute(fun))
  colsc=unlist(str_split(cols,pattern=" "))
  cc=NULL
  for (c in colsc){
    cc=c(cc,names(DT)[which(str_detect(names(DT),glob2rx(c)))])
  }
  colsc= cc
  eval(substitute(DT[filter,colsc:=lapply(.SD,function(x){fun(x,...)}),by,.SD=colsc]))
}

