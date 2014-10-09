#' Retain certain columns of a data.table in place (= Stata keep).
#'
#' @param x a data.table 
#' @param ... Variables to keep. See the \link[dplyr]{select} documentation.
#' @examples
#' N <- 100; K <- 10
#' DT <- data.table(
#'   id = 1:N,
#'   v1 = sample(5, N, TRUE),
#'   v2 = sample(1e6, N, TRUE)
#' )
#' setcols(DT, id, v2)
#' setcols(DT, -id)
#' @export
setcols <- function(x, ...){
	setcols_(x = x, .dots = lazyeval::lazy_dots(...))
}

#' @export
#' @rdname setcols
setcols_ <- function(x, ..., .dots){
	stopifnot(is.data.table(x))
	dots <- lazyeval::all_dots(.dots, ...)
	vars <- names(select_vars_(names(x), dots))
	if (!length(vars)) stop("No variable selected")
	drop <- setdiff(names(x), vars)
	x[, (drop) := NULL]
}




