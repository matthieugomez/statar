#' Set for Stata commands that don't modify datasets
#' 
#' @param DT A data.table.
#' @param cmd  One stata commandout of the following: summarize. Abbreviations are accepted.
#' @param cols A character vector of columns on which to apply the command.
#' @param ... Options to pass to the datata command.
#' DT %>% edo(summarize, "v2")
#' DT %>% edo(sum, "v*", d = TRUE)
#' @export
edo=function(DT,fun,cols=names(DT),...,i=NULL,by=NULL){
  if (!is.data.table(DT)){
    stop(paste0("First argument is not a data.table. Convert it first using setDT()"))
  }
  cmdc=as.character(substitute(cmd))
  cmdc <-match.arg(cmdc,c("summarize"))
  options=eval(substitute(alist(...)))
  colsub = substitute(cols)
  colvars = idvars_q(colsub,names(DT))
  if (funclist=="summarize"){
      eval(substitute(invisible(DT[,describe(.SD,d),.SDcols=colvars,...])))
  }
}
