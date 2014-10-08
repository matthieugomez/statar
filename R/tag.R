#' Tag a numeric vector (=Stata tag)
#' 
#' @param x A vector of values
#' @return A vector with only 1
#' @examples                        
#' v <- c(1, 4, 6, 99)                      
#' tag(v)
#' tag(v, fromLast = TRUE)
tag <- function(x, fromLast = FALSE){
  out <- rep(0, length(x))
  if (!fromLast){
    return(c(1, rep(0, length(x)-1)))
  } else{
    return(c(rep(0, length(x)-1),1))
  }
}