#' Keep only certain columns in place 
#'
#' @param x a data.table 
#' @param ... Variables to keep. Default to all. See the \link[dplyr]{select} documentation.
#' @param vars Used to work around non-standard evaluation.
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
setkeep <- function(x, ...){
	setkeep_(x = x, vars = lazyeval::lazy_dots(...))
}

#' @export
#' @rdname setkeep
setkeep_ <- function(x, vars){
	stopifnot(is.data.table(x))
	dots <- lazyeval::all_dots(vars)
	vars <- names(select_vars_(names(x), dots))
	if (!length(vars))  vars <- names(x)
	drop <- setdiff(copy(names(x)), vars)
	if (length(drop)>0){
		x[, c(drop) := NULL]
	}
}



#' Drop certain columns in place 
#'
#' @param x a data.table 
#' @param ... Variables to keep. Default to all. See the \link[dplyr]{select} documentation.
#' @param vars Used to work around non-standard evaluation.
#' @examples
#' library(data.table)
#' DT <- data.table(
#'   id = c(1,2),
#'   v1 = c(1,1),
#'   v2 = c(2,1)
#' )
#' setdrop(DT, id)
#' @export
setdrop <- function(x, ...){
	setdrop_(x = x, vars = lazyeval::lazy_dots(...))
}
#' @export
#' @rdname setdrop
setdrop_ <- function(x, vars){
	stopifnot(is.data.table(x))
	dots <- lazyeval::all_dots(vars)
	vars <- names(select_vars_(names(x), dots))
	if (!length(vars))  vars <- names(x)
	if (length(drop)>0){
		x[, c(vars) := NULL]
	}
}


##' Create new data.table by keeping only certain columns or rows
##'
##' @param x a data.table 
##' @param ... Variables to keep. Default to all. See the \link[dplyr]{select} documentation.
##' @param vars Used to work around non-standard evaluation.
##' @param i Condition within groups
##' @param by Groups by which condition should be evaluated
##' @examples
##' library(data.table)
##' DT <- data.table(
##'   id = c(1,2),
##'   v1 = c(1,1),
##'   v2 = c(2,1)
##' )
##' keep(DT, i = id==1)
##' keep(DT, id, v2)
##' keep(DT, -id)
##' @export
# keep <- function(x, ...,i = NULL, by = NULL){
# 	keep_(x = x, vars = lazyeval::lazy_dots(...), i = substitute(i), by = substitute(by))
# }
# 
# #' @export
# #' @rdname keep
# keep_ <- function(x, vars, i = NULL, by = NULL){
# 		stopifnot(is.data.table(x))
# 		byvars <- names(select_vars_(names(x), by))
# 		if (!length(by)){
# 		    byvars <- NULL
# 		}
# 		dots <-  lazyeval::all_dots(vars)
# 		vars <- names(select_vars_(names(x), dots))
# 		if (!length(vars))  vars <- names(x)
# 		if (!is.null(i)){
# 			if (is.null(by)){
# 				x <-  filter_(x, i)
# 			} else{
# 				expr <- lapply(dots, `[[`, "expr")
# 				call <- substitute(dt[, .I[expr], by = vars], list(expr=expr))
# 				env <- dt_env(x, lazyeval::common_env(dots), byvars = byvars)
# 				ans <- eval(call, env)
# 				indices <- ans[[length(ans)]]
# 				x <-  x[indices[!is.na(indices)]]
# 			}
# 		}
# 		drop <- setdiff(copy(names(x)), vars)
# 		if (length(drop)>0){
# 			x <- select_(x, .dots = vars)
# 		}
# 	x
# 	}



