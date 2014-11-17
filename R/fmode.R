#' Statistical mode
#' 
#' @param x A vector of values
#' @param na.rm logical. Should NA ignored? Default to TRUE. If FALSE, NA are considered as a value per se.
#' @param ties A character among "all", "min" and "max"
#' @return Returns one mode of the vector
#' @examples                        
#' fmode(c(1, 2, 2))
#' fmode(c(1, 2), ties = "min")
#' fmode(c(1, 2), ties = "max")
#' fmode(c(1, 2), ties = "all")
#' fmode(c(NA, NA, 1))
#' fmode(c(NA, NA, 1), na.rm = FALSE)
#' @export
fmode <- function(x, na.rm = TRUE, ties.method = c("min", "max", "all")) {
	ties.method <- match.arg(ties.method, c("min", "max", "all"))
	order <- data.table:::forderv(x, retGrp = TRUE)
	start <- attr(order,"starts")
	size_groups <- diff(c(attr(order,"starts"), length(x) + 1))
	n <- 0
	if (!length(order)){
		order <- seq_along(x)
	}
	if (na.rm){
		if (is.na(x[order[1]])){
			size_groups[1] <- -1
		}
	}
	if (ties.method == "min"){
	  out <- x[order[start[which.max(size_groups)]]]
	} else{
		z <- which(size_groups == max(size_groups))
		out <- x[order[start[z]]]
		if (ties.method == "all"){
		} else{
			out <- out[length(out)]
		}
	}
	out
}

#' @export
#' @rdname fmode
sample_mode <-  function(x, na.rm = TRUE, ties.method = "min") {
	fmode(x = x, na.rm = na.rm, ties.method = ties.method)
}