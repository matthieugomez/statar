
eset=function(DT,fun,cols=names(DT),...,i=NULL,by=NULL,d=FALSE){
  func=as.character(substitute(fun))
  func <-match.arg(func,c("sort","order","rename","summarize","balance"))
  options=eval(substitute(alist(...)))
  colsub = substitute(cols)
  colvars = idvars_q(colsub,names(DT))
  if (colm) ansvars = setdiff(names(DT), cols) else ansvars = cols
  if (func=="sort"){
    eval(substitute(setkeyv(DT,ansvars,...)))
  }
  if (func=="order"){
    eval(substitute(setcolorder(DT,c(ansvars,setdiff(names(DT),ansvars),...))))
  }
  if (func=="rename"){
    eval(substitute(setnames(DT,ansvars,...)))
  }
  if (func=="keep"){
    if (colm) ansvars = setdiff(names(DT), ansvars) else ansvars = ansvars
      DT[,(ansvars):=NULL] 
  }

  if (func=="summarize"){
      eval(substitute(DT[,describe(.SD,d),.SDcols=ansvars,...]))
  }

  if (func=="balance"){
  
  }
}
