##' Sample mode
##' 
##' @param x A vector of values
##' @param na.rm logical. Should NA ignored? Default to TRUE. If FALSE, NA are considered as a value per #se.
##' @param ties.method A character among "all", "min" and "max" that changes behavior in case of multiple #modes. Default to "min".
##' @return Returns one mode of the vector
##' @examples                        
##' sample_mode(c(1, 2, 2))
##' sample_mode(c(1, 2), ties = "min")
##' sample_mode(c(1, 2), ties = "max")
##' sample_mode(c(1, 2), ties = "all")
##' sample_mode(c(NA, NA, 1))
##' sample_mode(c(NA, NA, 1), na.rm = FALSE)
##' @export
##' @name sample mode
#sample_mode <- function(x, na.rm = TRUE, ties.method = c("min", "max", "all")) {
#	ties.method <- match.arg(ties.method, c("min", "max", "all"))
#	order <- forderv(x, retGrp = TRUE)
#	start <- attr(order, "starts")
#	size_groups <- diff(c(attr(order, "starts"), length(x) + 1))
#	n <- 0
#	if (!length(order)){
#		order <- seq_along(x)
#	}
#	if (na.rm){
#		if (is.na(x[order[1]])){
#			size_groups[1] <- -1
#		}
#	}
#	if (ties.method == "min"){
#	  out <- x[order[start[which.max(size_groups)]]]
#	} else{
#		z <- which(size_groups == max(size_groups))
#		out <- x[order[start[z]]]
#		if (ties.method == "max"){
#		out <- out[length(out)]
#		}
#	}
#	out
#}
