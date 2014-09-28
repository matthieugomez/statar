#' Fill Absent observations
#' @param .data A tbl_dt, grouped or not
#' @param ... Variables to expand
#' @examples
#' library(data.table)
#' library(dplyr)
#' DT <- data.table(
#'  id = c(1, 1, 1, 1, 1, 2, 2), 
#'  date = c(1992, 1989, 1991, 1990, 1994, 1992, 1991), 
#'  value = c(4.1, 4.5, 3.3, 5.3, 3.0, 3.2, 5.2)
#')
#' DT <- DT %>% group_by(id) %>% expand(date)
#' DT <- DT %>% expand(date)
#' @export
expand.data.table <- function(.data, ...) {
  expand_(.data, .dots = lazyeval::lazy_dots(...))
}

#' @export
expand.tbl_dt <- function(.data, ...) {
  expand_(.data, .dots = lazyeval::lazy_dots(...))
}

#' @export
expand.grouped_dt <- function(.data, ...) {
  expand_(.data, .dots = lazyeval::lazy_dots(...))
}

#' @export
#' @rdname expand
expand_ <- function(.data, ...,.dots) {
  UseMethod("expand_")
}

#' @export
expand_.grouped_dt <- function(.data,...,.dots){
  dots <- lazyeval::all_dots(.dots, ...)
  var_name <- names(dplyr::select_vars_(names(.data), dots))
  byvars <- dt_env(.data, lazyeval::common_env(dots))$vars
  for (t in var_name) {
    setkeyv(.data,c(byvars,t))
    call <- substitute(.data[, list(seq.int(t[1], t[.N])), by = c(byvars)], list(t = as.name(t)))
    ans  <- eval(call)
    setnames(ans, c(byvars, t))
    setkeyv(ans, c(byvars, t))
    .data <- .data[ans,allow.cartesian=TRUE]
  }
  .data
}

#' @export
expand_.data.table <- function(.data,...,.dots){
  dots <- lazyeval::all_dots(.dots, ...)
  var_name <- names(dplyr::select_vars_(names(.data), dots))
  env <- dt_env(.data, lazyeval::common_env(dots))
  for (t in var_name) {
    setkeyv(.data,c(t))
    call <- substitute(.data[, list(seq.int(t[1], t[.N]))], list(t = as.name(t)))
    ans  <- eval(call)
    setnames(ans, c(t))
    setkeyv(ans, c(t))
    .data <- .data[ans,allow.cartesian=TRUE]
  }
  .data
}


#' @export
expand_.tbl_dt <- function(.data, ..., .dots) {
  tbl_dt(NextMethod(), copy = FALSE)
}





