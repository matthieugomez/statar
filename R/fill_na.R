#' fill NA based on non missing observations
#'
#' @param .data a data.table 
#' @param ... variables to fill in. Default to every variable except grouped and order_by
#' @param order_by a variable along with observations should be filled
#' @param roll When roll is a positive number, this limits how far values are carried forward. roll=TRUE is equivalent to roll=+Inf. When roll is a negative number, values are rolled backwards; i.e., next observation carried backwards (NOCB). Use -Inf for unlimited roll back. When roll is "nearest", the nearest value is joined to.
#' @param rollend  A logical vector length 2 (a single logical is recycled). When rolling forward (e.g. roll=TRUE) if a value is past the last observation within each group defined by the join columns, rollends[2]=TRUE will roll the last value forwards. rollends[1]=TRUE will roll the first value backwards if the value is before it. If rollends=FALSE the value of i must fall in a gap in x but not after the end or before the beginning of the data, for that group defined by all but the last join column. When roll is a finite number, that limit is also applied when rolling the end
#' @param inplace Should the variable modified in place ? Default to FALSE.
#' @examples
#' DT <- data.table(
#'  id    = c(1, 1, 1, 1, 1, 2, 2),
#'  date  = c(1992, 1989, 1991, 1990, 1994, 1992, 1991),
#'  value = c(4.1, NA, NA, 5.3, 3.0, 3.2, 5.2)
#' )
#' DT %>% group_by(id) %>% fill_na(value, order_by = date)
#' DT %>% group_by(id) %>% fill_na(value, order_by = date, inplace = TRUE)
#' @name fill_na
NULL

#' @export
#' @rdname fill_na
fill_na <- function(.data, ..., order_by, roll = TRUE ,  rollends = if (roll=="nearest") c(TRUE,TRUE)
  else if (roll>=0) c(FALSE,TRUE)
  else c(TRUE,FALSE), inplace = FALSE){
	fill_na_(.data, .dots = lazy_dots(...), order_by, roll = TRUE, rollends = rollends, inplace = inplace)
}

#' @export
#' @rdname fill_na
fill_na_ <- function(.data, ...,.dots, roll = TRUE ,  rollends = if (roll=="nearest") c(TRUE,TRUE)
  else if (roll>=0) c(FALSE,TRUE)
  else c(TRUE,FALSE), inplace = FALSE) {
  UseMethod("fill_na_")
}


#' @export
fill_na_.grouped_dt  <- function(.data, ...,.dots, order_by, roll = TRUE ,  rollends = if (roll=="nearest") c(TRUE,TRUE)
  else if (roll>=0) c(FALSE,TRUE)
  else c(TRUE,FALSE), inplace = FALSE){
	dots <- lazyeval::all_dots(.dots, ...)
	vars <- names(select_vars_(names(.data), dots))
	byvars <- as.character(groups(.data))
	if (length(vars) == 0) {
		vars <- setdiff(names(.data), c(byvars, order_by))
	}
	if (!inplace) .data <- copy(.data)
	keys <- key(.data)
	setkeyv(.data, c(byvars, order_by))
	for (col in vars){	
    eval(substitute(.data2[, (col) := .data2[!is.na(x), c(byvars, order_by, col), with = FALSE ][.data2, value, roll = roll, rollends = rollends]], list(x = as.name(col))))
  	}
  	if (inplace) setkeyv(.data, keys)
}

#' @export
fill_na_.data.table <- function(.data, ...,.dots, order_by, roll = TRUE ,  rollends = if (roll=="nearest") c(TRUE,TRUE)
  else if (roll>=0) c(FALSE,TRUE)
  else c(TRUE,FALSE), inplace = FALSE){
	dots <- lazyeval::all_dots(.dots, ...)
	vars <- names(select_vars_(names(.data), dots))
	if (length(vars) == 0) {
		vars <- setdiff(names(.data), order_by)
	}
	if (!inplace) .data <- copy(.data)
	keys <- key(.data)
	setkeyv(.data, c(order_by))
	for (col in vars){	
    eval(substitute(.data2[, (col) := .data2[!is.na(x), c(order_by, col), with = FALSE ][.data2, value, roll = roll, rollends = rollends]], list(x = as.name(col))))
  	}
  	if (inplace) setkeyv(.data, keys)
}


#' @export
fill_na_.tbl_dt <- function(.data, ..., .dots, along_with, roll = TRUE ,  rollends = if (roll=="nearest") c(TRUE,TRUE)
  else if (roll>=0) c(FALSE,TRUE)
  else c(TRUE,FALSE), inplace = FALSE) {
  tbl_dt(NextMethod(), copy = FALSE)
}





