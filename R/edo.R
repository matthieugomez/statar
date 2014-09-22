
edo=function(DT,fun,cols=names(DT),...,i=NULL,by=NULL,d=FALSE){
	if (!is.data.table(DT)){
	  setDT(DT)
	  message("Data.frame coerced to data.table")
	}
  func=as.character(substitute(fun))
  funclist <-match.arg(func,c("summarize"))
  colsub = substitute(cols)
  colvars = idvars_q(colsub,names(DT))
  if (funclist=="summarize"){
      eval(substitute(invisible(DT[,describe(.SD,d),.SDcols=colvars,...])))
  }
}
