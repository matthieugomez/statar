#' Creates a vector of zero except for one subscript
#' 
#' @param n An integer (size of output)
#' @param fromLast Should 1 be at the end?
#' @return A vector of zeros except one subscript equal to one.
#' @examples                        
#' tag(2)
#' tag(2, fromLast = TRUE)
#' @export
tag <- function(n, fromLast = FALSE){
  out <- rep(0, n)
  if (!fromLast){
    out[1] <-1
  } else{
  	out[n]<- 1
  }
  out
}