#' Statistical mode
#' 
#' @param x A vector of values
#' @param na.rm Should NA values dropped?
#' @return Returns one mode of the vector (in case of ties, the first value is chosen)
#' @examples                        
#' sample_mode(c(1, 2, 2))
#' sample_mode(c(1, 2))
#' sample_mode(c(NA,NA,1))
#' sample_mode(c(NA,NA,1), na.rm = TRUE)
#' @export
sample_mode <- function(x, na.rm = FALSE) {
  if (na.rm) x <- x[!is.na(x)]
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}