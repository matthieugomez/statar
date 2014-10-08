#' keep columns
#'
#' @param .data a data.table 
#' @param cols a character vector of variable names to keep
#' @examples
#' DT <- data.table(
#'  id    = c(1, 1, 1, 1, 1, 2, 2),
#'  date  = c(1992, 1989, 1991, 1993, 1994, 1992, 1991),
#'  value = c(NA, NA, 3, 5.3, 3.0, 3.2, 5.2)
#' )
#' setcols(DT, c("id", "date"))
#' @export
setcols <- function(.data, cols){
	drop <- setdiff(names(.date),cols)
	.data[, (drop) := NULL]
}




