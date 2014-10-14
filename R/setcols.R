#' Retain certain columns of a data.table in place (= Stata keep).
#'
#' @param x a data.table 
#' @param ... Variables to keep. See the \link[dplyr]{select} documentation.
#' @param .dots Used to work around non-standard evaluation.
#' @examples
#' library(data.table)
#' DT <- data.table(
#'   id = c(1,2),
#'   v1 = c(1,1),
#'   v2 = c(2,1)
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




