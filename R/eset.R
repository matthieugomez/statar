#' Set of commands that replace data.table in place
#' 
#' @param DT A data.table.
#' @param cmd  One stata commandout of the following: sort, order, rename, keep, drop. Abbreviations are accepted.
#' @param cols A character vector of columns on which to apply the command.
#' @export
eset=function(DT,cmd,cols=names(DT),...,i=NULL,by=NULL,d=FALSE){
  if (!is.data.table(DT)){
    setDT(DT)
    message("Data.frame coerced to data.table")
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

}
