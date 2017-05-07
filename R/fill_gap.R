#'  Add rows corresponding to gaps in some variable
#'
#' @param x A data frame
#' @param ... a time variable
#' @param .dots Used to work around non standard evaluation
#' @param full  A boolean. When full = FALSE (default) rows are filled with respect to min and max of \code{...} within each group. When full = TRUE, rows are filled with respect to min and max of \code{...} in the whole datasets. 
#' @param roll When roll is a positive number, this limits how far values are carried forward. roll=TRUE is equivalent to roll=+Inf. When roll is a negative number, values are rolled backwards; i.e., next observation carried backwards (NOCB). Use -Inf for unlimited roll back. When roll is "nearest", the nearest value is used.
#' @param rollends  A logical vector length 2 (a single logical is recycled). When rolling forward (e.g. roll=TRUE) if a value is past the last observation within each group defined by the join columns, rollends[2]=TRUE will roll the last value forwards. rollends[1]=TRUE will roll the first value backwards if the value is before it. If rollends=FALSE the value of i must fall in a gap in x but not after the end or before the beginning of the data, for that group defined by all but the last join column. When roll is a finite number, that limit is also applied when rolling the end
#' @examples
#' library(dplyr)
#' library(lubridate)
#' df <- data_frame(
#'     id    = c(1, 1, 1, 2),
#'     datem  = as.monthly(mdy(c("04/03/1992", "01/04/1992", "03/15/1992", "05/11/1992"))),
#'     value = c(4.1, 4.5, 3.3, 3.2)
#' )
#' df %>% group_by(id) %>% fill_gap(datem)
#' df %>% group_by(id) %>% fill_gap(datem, full = TRUE)
#' df %>% group_by(id) %>% fill_gap(datem, roll = "nearest")
#' df %>% group_by(id) %>% fill_gap(datem, roll = "nearest", full = TRUE)
#' @export
fill_gap <- function(x, ...,  full = FALSE, roll = FALSE, rollends = if (roll=="nearest") c(TRUE,TRUE)
	else if (roll>=0) c(FALSE,TRUE)
	else c(TRUE,FALSE)) {
	byvars <- dplyr::group_vars(x)
	timevar <- setdiff(names(dplyr::select_vars(names(x), ...)), byvars)
	if (length(timevar) > 1) {
	    message("There should only be one variable for time")
	}
	originalattributes <- attributes(x)$class

	# check byvars, timevar form a panel
	stopifnot(is.panel(x, !!as.name(timevar)))

	# create id x time 
	ans <- dplyr::select_at(x, c(byvars, timevar))
	setDT(ans)
	if (!full){
		ans <- lazy_eval(interp(~ans[, list(seq(min(v), max(v), by = 1L)), by = c(byvars)], v = as.name(timevar)))
	}
	else{
		a <- min(ans[[timevar]])
		b <- max(ans[[timevar]])
		ans <- lazy_eval(interp(~ans[, list(seq(a, b, by = 1L)), by = c(byvars)], a = a, b = b))
	}
	setnames(ans, c(byvars, timevar))
	for (name in names(attributes(get(timevar, x)))){
		setattr(ans[[timevar]], name, attributes(get(timevar, x))[[name]]) 
	}

	# data.table merge with roll
	setkeyv(ans, c(byvars, timevar))
	x <- as.data.table(x)
	setkeyv(x, c(byvars, timevar))
	out <- x[ans, allow.cartesian = TRUE, roll = roll, rollends = rollends]

	# re assign group and class attributes
	out <- dplyr::group_by_at(out, byvars)
	setattr(out, "class", originalattributes)
	out
}