#'  Add rows corresponding to gaps in some variable
#'
#' @param x A data frame
#' @param ... a time variable
#' @param full  A boolean. When full = FALSE (default), the function creates rows corresponding to all missing times between the min and max of \code{...} within each group. When full = TRUE, the function creates rows corresponding to all missing times between the min and max of \code{...} in the whole dataset. 
#' @param roll When roll is a positive number, values are carried forward. roll=TRUE is equivalent to roll=+Inf. When roll is a negative number, values are rolled backwards; i.e., next observation carried backwards (NOCB). Use -Inf for unlimited roll back. When roll is "nearest", the nearest value is used. Default to FALSE (no rolling)
#' @param rollends  A logical vector length 2 (a single logical is recycled). When rolling forward (e.g. roll=TRUE) if a value is past the last observation within each group defined by the join columns, rollends[2]=TRUE will roll the last value forwards. rollends[1]=TRUE will roll the first value backwards if the value is before it. If rollends=FALSE the value of i must fall in a gap in x but not after the end or before the beginning of the data, for that group defined by all but the last join column. When roll is a finite number, that limit is also applied when rolling the end
#' @examples
#' library(dplyr)
#' library(lubridate)
#' df <- tibble(
#'     id    = c(1, 1, 1, 1),
#'     datem  = as.monthly(mdy(c("01/01/1992", "02/01/1992", "04/01/1992", "7/11/1992"))),
#'     value = c(4.1, 4.5, 3.3, 3.2)
#' )
#' df %>% group_by(id) %>% fill_gap(datem)
#' df %>% group_by(id) %>% fill_gap(datem, roll = 1)
#' df %>% group_by(id) %>% fill_gap(datem, roll = "nearest")
#' df %>% group_by(id) %>% fill_gap(datem, roll = "nearest", full = TRUE)
#' @export
fill_gap <- function(x, ...,  full = FALSE, roll = FALSE, rollends = if (roll=="nearest") c(TRUE,TRUE)
	else if (roll>=0) c(FALSE,TRUE)
	else c(TRUE,FALSE)) {
	byvars <- dplyr::group_vars(x)
	timevar <- setdiff(names(tidyselect::vars_select(names(x), ...)), byvars)
	if (length(timevar) > 1) {
		message("There should only be one variable for time")
	}
	originalattributes <- attributes(x)$class
	# check byvars, timevar form a panel
	stopifnot(is.panel(x, .data[[timevar]]))
	# create id x time 
	ans <- dplyr::select(x, dplyr::all_of(byvars), .data[[timevar]])
	data.table::setDT(ans)
	print(timevar)
	if (!full){
		ans <- lazyeval::lazy_eval(lazyeval::interp(~ans[, list(seq(min(v), max(v), by = 1L)), by = c(byvars)], v = as.name(timevar)))
	}
	else{
		a <- min(ans[[timevar]])
		b <- max(ans[[timevar]])
		ans <- lazyeval::lazy_eval(lazyeval::interp(~ans[, list(seq(a, b, by = 1L)), by = c(byvars)], a = a, b = b))
	}
	print("ok3")
	data.table::setnames(ans, c(byvars, timevar))
	for (name in names(attributes(get(timevar, x)))){
		data.table::setattr(ans[[timevar]], name, attributes(get(timevar, x))[[name]]) 
	}
	print(ans)

	# data.table merge with roll
	data.table::setkeyv(ans, c(byvars, timevar))
	x <- data.table::as.data.table(x)
	data.table::setkeyv(x, c(byvars, timevar))
	out <- x[ans, allow.cartesian = TRUE, roll = roll, rollends = rollends]

	# re assign group and class attributes
	out <- dplyr::group_by(out, dplyr::across(dplyr::all_of(byvars)))
	data.table::setattr(out, "class", originalattributes)
	out
}