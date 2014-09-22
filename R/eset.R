
eset=function(DT,fun,cols=names(DT),...,i=NULL,by=NULL,d=FALSE){
  func=as.character(substitute(fun))
  func <-match.arg(func,c("sort","order","rename","summarize","balance"))
  options=eval(substitute(alist(...)))
  colsub = substitute(cols)
  colvars = idvars_q(colsub,names(DT))
  if (func=="sort"){
    eval(substitute(setkeyv(DT,colvars,...)))
  }
  if (func=="order"){
    eval(substitute(setcolorder(DT,c(colvars,setdiff(names(DT),colvars),...))))
  }
  if (func=="rename"){
    eval(substitute(setnames(DT,colvars,...)))
  }
  if (func=="keep"){
    colvars = setdiff(names(DT), colvars)
    DT[,(colvars):=NULL] 
  }

  if (func=="summarize"){
      eval(substitute(DT[,describe(.SD,d),.SDcols=colvars,...]))
  }

  if (func=="balance"){
  
  }
}
