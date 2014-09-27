#' Set of commands that don't replace data.table in place
#' 
#' @param DT A data.table.
#' @param cmd  One stata commandout of the following: summarize. Abbreviations are accepted.
#' @param cols A character vector of columns on which to apply the command.
#' @export
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
