gen=function(DT,fun,cols=names(DT),newcols,i=TRUE,by=NULL,...){
  stopifnot(length(cols)==length(newcols) & !is.element(newcols,names(DT))==FALSE)
  func=as.character(substitute(fun))
  colsc=unlist(str_split(cols,pattern=" "))
  cc=NULL
  for (c in colsc){
    cc=c(cc,names(DT)[which(str_detect(names(DT),glob2rx(c)))])
  }
  colsc= cc
  if (is.character(fun)|is.numeric(fun)|is.integer(fun)|is.logical(fun)){
      eval(substitute(DT[i,newcols:=lapply(.SD,function(x){fun}),by,.SD=colsc]))
    } else{  
      eval(substitute(DT[i,newcols:=lapply(.SD,function(x){fun(x,...)}),by,.SD=colsc]))
    }
}

replace=function(DT,fun,cols=names(DT),i=TRUE,by=NULL,...){
  func=as.character(substitute(fun))
  colsc=unlist(str_split(cols,pattern=" "))
  cc=NULL
  for (c in colsc){
    cc=c(cc,names(DT)[which(str_detect(names(DT),glob2rx(c)))])
  }
  colsc= cc
  if (is.character(fun)|is.numeric(fun)|is.integer(fun)|is.logical(fun)){
      eval(substitute(DT[i,colsc:=lapply(.SD,function(x){fun}),by,.SD=colsc]))
    } else{  
      eval(substitute(DT[i,colsc:=lapply(.SD,function(x){fun(x,...)}),by,.SD=colsc]))
    }
}
