#' Set of Stata commands that don't modify datasets
#' 
#' @param DT A data.table.
#' @param cmd  One stata commandout of the following: summarize. Abbreviations are accepted.
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
#' DT %>% edo(summarize, "v2")
#' DT %>% edo(sum, "v*", d = TRUE)
#' @export
edo=function(DT,cmd,cols=names(DT),...,i=NULL,by=NULL){
  if (!is.data.table(DT)){
    stop(paste0("First argument is not a data.table. Convert it first using setDT()"))
  }
  cmdc=as.character(substitute(cmd))
  cmdc <-match.arg(cmdc,c("summarize"))
  options=eval(substitute(alist(...)))
  colsub = substitute(cols)
  colvars = idvars_q(colsub,names(DT))
  if (cmdc=="summarize"){
      eval(substitute(invisible(DT[,describe(.SD,d),.SDcols=colvars,...])))
  }
}
