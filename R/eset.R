#' Set for Stata commands that modify the dataset
#' @param DT A data.table.
#' @param cmd  One stata commandout of the following: sort, order, rename, keep, drop. Abbreviations are accepted.
#' @param cols A character vector of columns on which to apply the command.
#' @param ... Options to pass to the datata command.
#' DT %>% eset(order, cols = "v*")
#' DT %>% eset(sort, c("v1", "v2"))
#' DT %>% eset(rename, "v1", "v11")
#' DT %>% eset(keep, -"id*")
#' DT %>% eset(keep, "v?")
#' @export
eset=function(DT,fun,cols=names(DT),...,i=NULL,by=NULL){
  if (!is.data.table(DT)){
    stop(paste0("First argument is not a data.table. Convert it first using setDT()"))
  }
  cmdc=as.character(substitute(cmd))
  cmdc <-match.arg(cmdc,c("sort","order","rename","drop","keep"))
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
}
