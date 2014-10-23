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
#' setkeep(DT, id, v2)
#' setkeep(DT, -id)
#' @export
setkeep <- function(x, ...,i = NULL, by = NULL){
	setkeep_(x = x, .dots = lazyeval::lazy_dots(...), i = substitute(i), by = substitute(by))
}

#' @export
#' @rdname setkeep
setkeep_ <- function(x, ..., .dots, i = NULL, by = NULL){
	stopifnot(is.data.table(x))
	byvars <- names(select_vars_(names(x), by))
	if (!length(by)){
	    byvars <- NULL
	}
	dots <- lazyeval::all_dots(.dots, ...)
	vars <- names(select_vars_(names(x), dots))
	if (!length(vars)) stop("No variable selected")
	if (!is.null(i)){
		if (is.null(by)){
			x <- filter_(x, i)
		} else{
			expr <- lapply(dots, `[[`, "expr")
			call <- substitute(dt[, .I[expr], by = vars], list(expr = dplyr:::and_expr(expr)))
			env <- dt_env(x, lazyeval::common_env(dots), by = byvars)
			ans <- eval(call, env)
			indices <- ans[[length(ans)]]
			x <- x[indices[!is.na(indices)]]
		}
	}
	
	drop <- setdiff(copy(names(x)), vars)
	x[, c(drop) := NULL]
	x[]

}




and_expr <- function(exprs) {
  assert_that(is.list(exprs))
  if (length(exprs) == 0) return(TRUE)
  if (length(exprs) == 1) return(exprs[[1]])

  left <- exprs[[1]]
  for (i in 2:length(exprs)) {
    left <- substitute(left & right, list(left = left, right = exprs[[i]]))
  }
  left
}


f <- function(x){
	d <- "v1"
	x[, c(d) := NULL]
}