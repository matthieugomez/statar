#' Count number of non missing observations
#' 
#' @param ... A set of vectors
#' @examples                          
#' count(1:100, c(NA, 1:99))
#' @export
count <- function(...){
    sum(complete.cases(...))
  }
