#' Tag a numeric vector (= Stata tag)
#' 
#' @param x A vector of values
#' @return A vector with only 1
#' @examples                        
#' tag(c(1,2))
#' tag(c(1,2), fromLast = TRUE)
#' @export
tag <- function(x, fromLast = FALSE){
  out <- rep(0, length(x))
  if (!fromLast){
    return(c(1L, rep(0L, length(x)-1)))
  } else{
    return(c(rep(0L, length(x)-1),1))
  }
}