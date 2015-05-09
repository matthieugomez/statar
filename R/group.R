#' Create id for each group by order of appearance
#' 
#' @param x A vector, a list of vector, or a data.frame
#' @examples                        
#' group(list(c(1, 2, 2, 2), c(4, 4, 5, 4)))
#' @export
group <- function(x) {
	x <- as.data.frame(x)
	 setDT(x)
	 temp <- tempname(x)
	 x[, c(temp) :=  .GRP, by = names(x)]
	 x[[length(x)]]
}