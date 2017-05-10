#' Count number of non missing observations
#' 
#' @param ... a sequence of vectors, matrices and data frames.
#' @examples                          
#' n_narm(1:100, c(NA, 1:99))
#' @export
n_narm <- function(...){
    sum(complete.cases(...))
  }
