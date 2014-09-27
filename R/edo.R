#' Set of Stata commands
#' 
#' @param DT A data.table.
#' @param cmd  One stata commandout of the following: sort, order, rename, keep, drop, summarize. Abbreviations are accepted.
#' @param cols A character vector of columns on which to apply the command.
#' @param ... Options to pass to the datata command.
#' DT %>% edo(order, cols = "v*")
#' DT %>% edo(sort, c("v1", "v2"))
#' DT %>% edo(rename, "v1", "v11")
#' DT %>% edo(keep, -"id*")
#' DT %>% edo(keep, "v?")
#' DT %>% edo(summarize, "v2")
#' DT %>% edo(sum, "v*", d = TRUE)
#' @export
edo=function(DT,fun,cols=names(DT),...,i=NULL,by=NULL){
  if (!is.data.table(DT)){
    stop(paste0("First argument is not a data.table. Convert it first using setDT()"))
  }
  cmdc=as.character(substitute(cmd))
  cmdc <-match.arg(cmdc,c("sort","order","rename","summarize","keep"))
  options=eval(substitute(alist(...)))
  colsub = substitute(cols)
  colvars = idvars_q(colsub,names(DT))
  if (cmdc=="sort"){
    eval(substitute(setkeyv(DT,colvars,...)))
  }
  if (cmdc=="order"){
    eval(substitute(setcolorder(DT,c(colvars,setdiff(names(DT),colvars),...))))
  }
  if (cmdc=="rename"){
    eval(substitute(setnames(DT,colvars,...)))
  }
  if (cmdc=="keep"){
    colvars = setdiff(names(DT), colvars)
    DT[,(colvars):=NULL] 
  }
  if (cmdc=="drop"){
    colvars = colvars
    DT[,(colvars):=NULL] 
  }
  if (funclist=="summarize"){
      eval(substitute(invisible(DT[,describe(.SD,d),.SDcols=colvars,...])))
  }
}
