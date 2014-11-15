#' Statistical mode
#' 
#' @param x A vector of values
#' @param na.rm Should NA ignored? Default to TRUE
#' @param ties A character among "all", "min" and "max"
#' @return Returns one mode of the vector (in case of ties, the first value by order of appearance is chosen)
#' @examples                        
#' fmode(c(1, 2, 2))
#' fmode(c(1, 2), ties = "min")
#' fmode(c(1, 2), ties = "max")
#' fmode(c(1, 2), ties = "all")
#' fmode(c(NA, NA, 1))
#' fmode(c(NA, NA, 1), na.rm = FALSE)
#' @export
fmode <- function(x, na.rm = TRUE, ties.method = "min") {
	if (na.rm) x <- x[!is.na(x)]
	order <- data.table:::forderv(x, retGrp = TRUE)
	size <- diff(c(attr(order,"starts"), length(x) + 1))
	if (!length(order)){
		order <- seq.int(1, length(x))
	}
	if (ties.method == "min"){
	  out <- x[order[which.max(size)]]
	} else{
		z <- which(size == max(size))
		out <- x[order[z]]
		if (ties.method == "all"){
		} else{
			out <- out[length(out)]
		}
	}
	out
}

#' @export
#' @alias fmode
sample_mode <-  function(x, na.rm = TRUE, ties.method = "min") {
	fmode(x = x, na.rm = na.rm, ties.method = ties.method)
}