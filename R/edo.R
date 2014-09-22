
edo=function(DT,fun,cols=names(DT),...,i=NULL,by=NULL,d=FALSE){
  func=as.character(substitute(fun))
  funclist <-match.arg(func,c("summarize"))
  options=eval(substitute(alist(...)))
  colsub = substitute(cols)
  colvars = idvars_q(colsub,names(DT))
  if (funclist=="summarize"){
      eval(substitute(DT[,describe(.SD,d),.SDcols=ansvars,...]))
  }
}
