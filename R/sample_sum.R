#' sum
#' 
#' @param x A vector of values
#' @param na.rm logical. Should NA ignored? Default to TRUE. If FALSE, NA are considered as a value per se.
#' @param w vector of weights.
#' @param type type of weights. Can be aweight (then weight are transformed so that sum to length of non missinx x)
#' @param min If length of vector x once missing x and w and zero value of w are removed is strictly lower than min, the returned value is NA.
#' @return When w / min are not specified,  sample_sum(, min = 0) is the same as sum(., na.rm = TRUE) while sample_sum(, min = length(x)) is the same as sum(.).
#' @export
sample_sum <- function(x, w = rep(1L, length(x)), type = "aweight", min = 0) {
	index <- !is.na(x) & !is.na(w) & !(w == 0)
	if (length(index) < min){
		out <- NA
	} else{
		x <- x[index]
		w <- w[index]
		if (type == "aweight"){
			w <- w / sum(w) * length(x)
		}
		out <- sum(w * x)
	}
	out
}

