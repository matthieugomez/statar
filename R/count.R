#' Count number of non missing observations
#' 
#' @param ... a sequence of vectors, matrices and data frames.
#' @examples                          
#' count(1:100, c(NA, 1:99))
#' @export
count <- function(...){
    sum(complete.cases(...))
  }
