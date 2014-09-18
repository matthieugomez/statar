
gen_each=function(DT,fun,cols,gen,,...,i=TRUE,by=NULL){
  func=as.character(substitute(fun))
  colsc=unlist(str_split(cols,pattern=" "))
  cc=NULL
  for (c in colsc){
    cc=c(cc,names(DT)[which(str_detect(names(DT),glob2rx(c)))])
  }
 colsc=cc
 stopifnot(length(colsc)==length(gen) & !is.element(gen,names(DT)))
 eval(substitute(DT[i,gen:=lapply(.SD,funx,...),by,.SD=colsc]))
}

