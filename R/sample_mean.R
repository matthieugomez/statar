#' Sample mean
#' 
#' @param x A vector of values
#' @param w vector of weights.
#' @param min If length of vector x once missing x and w and zero value of w are removed is strictly lower than min, the returned value is NA.
#' @return  When w / min are not specified,  weighted_mean(, min = 0) is the same as mean(., na.rm = TRUE) while weighted_mean(, min = length(x)) is the same as mean(.).
#' @export
sample_mean <- function(x, na.rm = TRUE, w = rep(1L, length(x)), min = 0) {
	index <- !is.na(x) & !is.na(w) & !(w == 0)
	if (length(index) < min){
		out <- NA
	} else{
		x <- x[index]
		w <- w[index]
		w <- w / sum(w)
		out <- sum(w * x)
	}
	out
}

