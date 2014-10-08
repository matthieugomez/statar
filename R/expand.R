#'  Fill in gaps in variable
#'
#' @param .data A tbl_dt, grouped or not
#' @param ... Variables to keep
#' @param along_with integer variable to expand
#' @param type  "within"  means that rows are expanded with respect to min and max of \code{...} within groups (default) while "default" means that dates are expanded with respect to min and max of \code{...} across groups. 

#' @examples
#' library(data.table)
#' library(dplyr)
#' DT <- data.table(
#'  id = c(1, 1, 1, 1, 1, 2, 2), 
#'  date = c(1992, 1989, 1991, 1990, 1994, 1992, 1991), 
#'  value = c(4.1, 4.5, 3.3, 5.3, 3.0, 3.2, 5.2)
#')
#' DT %>% group_by(id) %>% expand(value, along_with = date)
#' DT %>% group_by(id) %>% expand(value, along_with = date, type == "across")
#' @name expand
NULL

#' @export
#' @rdname expand
expand <- function(.data, ..., along_with, type = c("within", "across")) {
  expand_(.data, .dots = lazyeval::lazy_dots(...), along_with = substitute(along_with), type = type)
}

#' @export
#' @rdname expand
expand_ <- function(.data, ..., along_with, .dots, type = c("within", "across")) {
  UseMethod("expand_")
}

#' @export
expand_.grouped_dt <- function(.data,...,along_with, .dots, type = c("within", "across")){
  along_with  <- names(select_vars_(names(.data), along_with ))
  dots <- lazyeval::all_dots(.dots, ...)
  byvars <- as.character(groups(.data))
  vars <- names(select_vars_(names(.data), dots, exclude = byvars))
  if (length(vars) == 0) {
     vars <- setdiff(names(.data),c(byvars, along_with))
  }
  type <- match.arg(type)
  isna <- eval(substitute(.data[,sum(is.na(t))], list(t = as.name(along_with))))
  if (isna>0) stop("Variable along_with has missing values",call. = FALSE)
  if (anyDuplicated(.data,c(byvars,along_with)) stop(paste(collapse(byvars,","),",",along_with,"do not uniquely identify observations"))_
  if (type=="within"){
    call <- substitute(.data[, list(seq.int(min(t, na.rm = TRUE), max(t, na.rm = TRUE))), by = c(byvars)], list(t = as.name(along_with)))
  } else{
    a <- eval(substitute(.data[,min(t, na.rm = TRUE)], list(t = as.name(along_with))))
    b <- eval(substitute(.data[,max(t, na.rm = TRUE)], list(t = as.name(along_with))))
    call <- substitute(.data[, list(seq.int(a, b)), by = c(byvars)], list(a = a, b=b))
  }
  ans  <- eval(call)
  setnames(ans, c(byvars, along_with))
  setkeyv(ans, c(byvars, along_with))
  .data <- .data[, c(byvars,along_with, vars), with = FALSE]
  setkeyv(.data, c(byvars,along_with))
  .data <- .data[ans,allow.cartesian=TRUE]
    .data
}

#' @export
expand_.data.table <- function(.data,..., along_with, .dots, type = c("within", "across")){
  along_with  <- names(select_vars_(names(.data), along_with ))
  dots <- lazyeval::all_dots(.dots, ...)
  vars <- names(select_vars_(names(.data), dots))
  if (length(vars) == 0) {
     vars <- setdiff(names(.data), along_with)
  }
  isna <- eval(substitute(.data[,sum(is.na(t))], list(t = as.name(along_with))))
  if (isna>0) stop("Variable along_with has missing values",call. = FALSE)
  if (anyDuplicated(.data,along_with)) stop(paste(along_with,"does not uniquely identify observations"))_
    setkeyv(.data,c(along_with))
    a <- eval(substitute(.data[,min(t, na.rm = TRUE)], list(t = as.name(along_with))))
    b <- eval(substitute(.data[,max(t, na.rm = TRUE)], list(t = as.name(along_with))))
    call <- substitute(.data[, list(seq.int(a, b))], list(a = a, b=b))
    ans  <- eval(call)
    setnames(ans, c(along_with))
    setkeyv(ans, c(along_with))
    .data <- .data[, c(vars, along_with), with = FALSE]
    setkeyv(.data, c(along_with))
    .data <- .data[ans,allow.cartesian=TRUE]
  .data
}


#' @export
expand_.tbl_dt <- function(.data, ..., along_with, .dots, type = c("within", "across")) {
  tbl_dt(NextMethod())
}





