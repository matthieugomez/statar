#' Set of Stata commands that modify datasets
#'
#' @param DT A data.table.
#' @param cmd  One stata commandout of the following: sort, order, rename, keep, drop. Abbreviations are accepted.
#' @param cols A character vector of columns on which to apply the command.
#' @param ... Options to pass to the datata command.
#' @examples
#' library(data.table)
#' library(dplyr)
#' N <- 100; K <- 10
#' DT <- data.table(
#'   id = 1:N,
#'   v1 =  sample(5, N, TRUE),                          
#'   v2 =  sample(1e6, N, TRUE),                       
#'   v3 =  sample(round(runif(100, max = 100), 4), N, TRUE) 
#' )
#' DT %>% eset(order, cols = "v*")
#' DT %>% eset(sort, c("v1", "v2"))
#' DT %>% eset(rename, "v1", "v11")
#' DT %>% eset(keep, -"id*")
#' DT %>% eset(keep, "v?")
#' @export
eset=function(DT,cmd,cols=names(DT),...,i=NULL,by=NULL){
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
