#'  Add rows corresponding to gaps in some variable
#'
#' @param .data A tbl_dt, grouped or not
#' @param ... Variables to keep
#' @param along_with integer variable to fill_gap
#' @param type  "within"  means that rows are fill_gaped with respect to min and max of \code{...} within groups (default) while "across" means that dates are fill_gaped with respect to min and max of \code{...} across groups. 
#' @param roll When roll is a positive number, this limits how far values are carried forward. roll=TRUE is equivalent to roll=+Inf. When roll is a negative number, values are rolled backwards; i.e., next observation carried backwards (NOCB). Use -Inf for unlimited roll back. When roll is "nearest", the nearest value is joined to.
#' @param rollend  A logical vector length 2 (a single logical is recycled). When rolling forward (e.g. roll=TRUE) if a value is past the last observation within each group defined by the join columns, rollends[2]=TRUE will roll the last value forwards. rollends[1]=TRUE will roll the first value backwards if the value is before it. If rollends=FALSE the value of i must fall in a gap in x but not after the end or before the beginning of the data, for that group defined by all but the last join column. When roll is a finite number, that limit is also applied when rolling the end
#' @examples
#' library(data.table)
#' library(dplyr)
#' DT <- data.table(
#'  id = c(1, 1, 1, 1, 1, 2, 2), 
#'  date = c(1992, 1989, 1991, 1990, 1994, 1992, 1991), 
#'  value = c(4.1, 4.5, 3.3, 5.3, 3.0, 3.2, 5.2)
#')
#' DT %>% group_by(id) %>% fill_gap(value, along_with = date)
#' DT %>% group_by(id) %>% fill_gap(value, along_with = date, type == "across")
#' @export
fill_gap <- function(.data, ..., along_with, type = c("within", "across"), roll = FALSE, rollends = if (roll=="nearest") c(TRUE,TRUE)
             else if (roll>=0) c(FALSE,TRUE)
             else c(TRUE,FALSE)) {
  fill_gap_(.data, .dots = lazyeval::lazy_dots(...), along_with = substitute(along_with), type = type, roll = roll, rollends = rollends)
}

#' @export
#' @rdname fill_gap
fill_gap_ <- function(.data, ..., along_with, .dots, type = c("within", "across"), roll = FALSE, rollends = if (roll=="nearest") c(TRUE,TRUE)
             else if (roll>=0) c(FALSE,TRUE)
             else c(TRUE,FALSE)) {
  UseMethod("fill_gap_")
}

#' @export
fill_gap_.grouped_dt <- function(.data,...,along_with, .dots, type = c("within", "across"), roll = FALSE, rollends = if (roll=="nearest") c(TRUE,TRUE)
             else if (roll>=0) c(FALSE,TRUE)
             else c(TRUE,FALSE)){
  along_with  <- names(select_vars_(names(.data), along_with ))
  dots <- lazyeval::all_dots(.dots, ...)
  byvars <- as.character(groups(.data))
  vars <- names(select_vars_(names(.data), dots, exclude = byvars))
  if (length(vars) == 0) {
     vars <- setdiff(names(.data),c(byvars, along_with))
  }
  type <- match.arg(type, c("within", "across"))
  isna <- eval(substitute(.data[,sum(is.na(t))], list(t = as.name(along_with))))
  if (isna>0) stop("Variable along_with has missing values" ,call. = FALSE)
  if (anyDuplicated(.data, by = c(byvars,along_with))) stop(paste0(paste(byvars, collapse = ","),", ",along_with," do not uniquely identify observations"), call. = FALSE)
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
fill_gap_.data.table <- function(.data,..., along_with, .dots, type = c("within", "across"), roll = FALSE, rollends = if (roll=="nearest") c(TRUE,TRUE)
             else if (roll>=0) c(FALSE,TRUE)
             else c(TRUE,FALSE)){
  along_with  <- names(select_vars_(names(.data), along_with ))
  dots <- lazyeval::all_dots(.dots, ...)
  vars <- names(select_vars_(names(.data), dots))
  if (length(vars) == 0) {
     vars <- setdiff(names(.data), along_with)
  }
  isna <- eval(substitute(.data[,sum(is.na(t))], list(t = as.name(along_with))))
  if (isna>0) stop("Variable along_with has missing values", call. = FALSE)
  if (anyDuplicated(.data, by = along_with)) stop(paste(along_with,"does not uniquely identify observations"), call. = FALSE)
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
fill_gap_.tbl_dt <- function(.data, ..., along_with, .dots, type = c("within", "across"), roll = FALSE, rollends = if (roll=="nearest") c(TRUE,TRUE)
             else if (roll>=0) c(FALSE,TRUE)
             else c(TRUE,FALSE)) {
  tbl_dt(NextMethod())
}





