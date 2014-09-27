#' Set of Stata commands that don't modify datasets
#' 
#' @param DT A data.table.
#' @param cmd  One stata commandout of the following: summarize. Abbreviations are accepted.
#' @param cols A character vector of columns on which to apply the command.
#' @param ... Options to pass to the stata command.
#' @param i Apply commands on certain rows
#' @param by Apply commands within groups

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
#' DT %>% edo(summarize, "v2")
#' DT %>% edo(sum, "v*", d = TRUE)
#' DT %>% edo(sum, "v*", i = v1 == 1)
#' DT %>% edo(sum, "v*", d = TRUE, by = "v1")
#' @export
edo=function(DT,cmd,cols=names(DT),...,i = TRUE,by = NULL){
  if (!is.data.table(DT)){
    stop(paste0("First argument is not a data.table. Convert it first using setDT()"))
  }
  cmdc=as.character(substitute(cmd))
  cmdc <-match.arg(cmdc,c("summarize"))
  options=eval(substitute(alist(...)))
  colsub = substitute(cols)
  colvars = idvars_q(colsub,names(DT))
  bysub <- substitute(by)
  byvars <- NULL
  if (length(bysub)) { byvars <- idvars_q(bysub,names(DT))}
  if (cmdc=="summarize"){
      eval(substitute(invisible(DT[i,describe(.SD,...), by = byvars, .SDcols = colvars])))
  }
}
