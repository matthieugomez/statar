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
  discard <- setdiff(names(x), vars)
  if (length(discard)>0){
    x[, c(discard) := NULL]
  }
}


#' discard certain columns in place 
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
#' setdiscard(DT, id)
#' @export
setdiscard <- function(x, ...){
  setdiscard_(x = x, vars = lazyeval::lazy_dots(...))
}
#' @export
#' @rdname setdiscard
setdiscard_ <- function(x, vars){
  stopifnot(is.data.table(x))
  dots <- lazyeval::all_dots(vars)
  vars <- names(select_vars_(names(x), dots))
  if (!length(vars))  vars <- names(x)
  if (length(discard)>0){
    x[, c(vars) := NULL]
  }
}


#' Create new data.table by keeping only certain columns (equivalent to dplyr::select)
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
#' keep(DT, id, v2)
#' keep(DT, -id)
#' @export
keep <- function(x, ...){
  keep_(x = x, vars = lazyeval::lazy_dots(...))
}
#' @export
#' @rdname keep
keep_ <- function(x, vars){
  stopifnot(is.data.table(x))
  dots <-  lazyeval::all_dots(vars)
  vars <- names(select_vars_(names(x), dots))
  if (!length(vars))  vars <- names(x)
  x[, vars, with = FALSE]
}


#' Create a new data.table by discarding certain columns 
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
#' discard(DT, id, v2)
#' discard(DT, -id)
#' @export
discard <- function(x, ...){
  discard_(x = x, vars = lazyeval::lazy_dots(...))
}

#' @export
#' @rdname discard
discard_ <- function(x, vars){
  stopifnot(is.data.table(x))
  dots <-  lazyeval::all_dots(vars)
  vars <- names(select_vars_(names(x), dots))
  keep <- setdiff(names(x), vars)
  x[, keep, with = FALSE]
}



#' Create new data.table by keeping only certain rows(equivalent to dplyr::filter)
#'
#' @param x a data.table 
#' @param ... Conditions
#' @param by groups in which the condition should be evaluated
#' @param .dots Used to work around non-standard evaluation.
#' @examples
#' library(data.table)
#' DT <- data.table(
#'   id = c(1,2,1),
#'   v1 = c(1,NA,2)
#' )
#' keep_if(DT, v1 == 1)
#' keep_if(DT, v1 == min(v1), by = id)
#' @export
keep_if <- function(x, ..., by = NULL){
  keep_if_(x = x, .dots = lazyeval::lazy_dots(...), by = substitute(by))
}

#' @export
#' @rdname keep_if
keep_if_ <- function(x, .dots, by = NULL){
  stopifnot(is.data.table(x))
  byvars <- names(select_vars_(names(x), by))
  if (!length(by)){
      byvars <- NULL
  }
  dots <-  lazyeval::all_dots(.dots)
  expr <- lapply(dots, `[[`, "expr")
  call <- substitute(dt[, .I[expr], by = byvars], list(expr=and_expr(expr)))
  env <- dt_env(x, lazyeval::common_env(dots), byvars = byvars)
  ans <- eval(call, env)
  indices <- ans[[length(ans)]]
  x[indices[!is.na(indices)]]
}



#' Create new data.table after discarding certain rows
#'
#' @param x a data.table 
#' @param ... Conditions. Rows where the condition evaluates to NA are not discardd. Therefore, \code{discard_if(dt, condition)} is not the same as \code{keep_if(x, !condition)} with 
#' @param by groups in which the condition should be evaluated
#' @param .dots Used to work around non-standard evaluation.
#' @examples
#' library(data.table)
#' DT <- data.table(
#'   id = c(1,2,1),
#'   v1 = c(1,NA,2)
#' )
#' discard_if(DT, v1 == 1)
#' discard_if(DT, v1 == 1, v1 == 2)
#' discard_if(DT, v1 == min(v1), by = id)
#' @export
discard_if <- function(x, ..., by = NULL){
  discard_if_(x = x, .dots = lazyeval::lazy_dots(...), by = substitute(by))
}

#' @export
#' @rdname discard_if
discard_if_ <- function(x, .dots, by = NULL){
  stopifnot(is.data.table(x))
  byvars <- names(select_vars_(names(x), by))
  if (!length(by)){
      byvars <- NULL
  }
  dots <-  lazyeval::all_dots(.dots)
  expr <- lapply(dots, `[[`, "expr")
  call <- substitute(dt[, .I[expr], by = byvars], list(expr=or_expr(expr)))
  env <- dt_env(x, lazyeval::common_env(dots), byvars = byvars)
  ans <- eval(call, env)
  indices <- ans[[length(ans)]]
  if (!length(indices)){
    out <- x
  } else{
    out <- x[-indices[!is.na(indices)]]
  }
  out
}


