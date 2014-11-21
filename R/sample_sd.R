#' Weighted sd
#' 
#' @param x A vector of values
#' @param w vector of weights.
#' @param type type of weights. Can be aweight (then weight are transformed so that sum to length of non missinx x)
#' @param min If length of vector x once missing x and w and zero value of w are removed is strictly lower than min, the returned value is NA.
#' @return When w / min are not specified,  weighted_sd(, min = 0) is the same as sd(., na.rm = TRUE) while weighted_sd(, min = length(x)) is the same as sd(.).
#' @export
weighted_sd <- function(x, w = rep(1L, length(x)), type = "aweight", min = 0) {
	index <- !is.na(x) & !is.na(w) & !(w == 0)
	if (length(index) < min){
		out <- NA
	} else{
		x <- x[index]
		w <- w[index]
		if (type == "aweight"){
			w <- w / sum(w) * length(x)
		}
		out <- sd(w * x)
	}
	out
}

