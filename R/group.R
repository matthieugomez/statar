#' Create id for each group by order of appearance
#' 
#' @param ... vectors
#' @examples                        
#' group(c(1, 2, 2), c(1,4,5))
#' @export
group <- function(...) {
	 x <- list(...)
	 setDT(x)
	 y <- x[, .GRP, by = names(x)]
	 y[[length(y)]]
}