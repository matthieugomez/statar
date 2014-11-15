#' Statistical mode
#' 
#' @param x A vector of values
#' @param na.rm Should NA removed?
#' @return Returns one mode of the vector (in case of ties, the first value by order of appearance is chosen)
#' @examples                        
#' sample_mode(c(1, 2, 2))
#' sample_mode(c(1, 2))
#' sample_mode(c(NA,NA,1))
#' sample_mode(c(NA,NA,1), na.rm = TRUE)
#' @export
sample_mode <- function(x, na.rm = TRUE) {
  if (na.rm) x <- x[!is.na(x)]
  order <- data.table:::forderv(x, retGrp = TRUE)
  size <- diff(c(attr(order,"starts"), length(x) + 1))
  if (!length(order)){
  	order <- seq.int(1, length(x))
  }
  x[order[which.max(size)]]
}