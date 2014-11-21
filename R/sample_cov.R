#' Sample covariance
#' 
#' @param x A vector of values
#' @param y A vector of values
#' @param min If length of vector x once missing x and w and zero value of w are removed is strictly lower than min, the returned value is NA.
#' @return Weighted covariance
#' @export
sample_cov <- function(x, y, w = rep(1L, length(x))) {
	index <- !is.na(x) & !is.na(y) & !is.na(w) & !(w == 0)
	if (length(index) < min){
		out <- NA
	} else{
		x <- x[index]
		y <- x[index]
		w <- w[index]
		out <- cov(x, y)
	}
	out
}

