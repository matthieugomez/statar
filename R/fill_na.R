#' fill NA based on non missing observations
#'
#' @param .data a data.table 
#' @param cols a character vector of variable names to fill-in
#' @param roll When roll is a positive number, this limits how far values are carried forward. roll=TRUE is equivalent to roll=+Inf. When roll is a negative number, values are rolled backwards; i.e., next observation carried backwards (NOCB). Use -Inf for unlimited roll back. When roll is "nearest", the nearest value is joined to.
#' @param rollend  A logical vector length 2 (a single logical is recycled). When rolling forward (e.g. roll=TRUE) if a value is past the last observation within each group defined by the join columns, rollends[2]=TRUE will roll the last value forwards. rollends[1]=TRUE will roll the first value backwards if the value is before it. If rollends=FALSE the value of i must fall in a gap in x but not after the end or before the beginning of the data, for that group defined by all but the last join column. When roll is a finite number, that limit is also applied when rolling the end
#' @examples
#' DT <- data.table(
#'  id    = c(1, 1, 1, 1, 1, 2, 2),
#'  date  = c(1992, 1989, 1991, 1990, 1994, 1992, 1991),
#'  value = c(4.1, NA, MA, 5.3, 3.0, 3.2, 5.2)
#' )
#' setkey(DT, id, date)
#' fill_na(DT, value, roll = TRUE, inplace = FALSE)
#' @name fill_na
NULL



fill_na <- function(.data, ...,roll = TRUE ,  rollends = if (roll=="nearest") c(TRUE,TRUE)
  else if (roll>=0) c(FALSE,TRUE)
  else c(TRUE,FALSE), inplace = FALSE){
	fill_na_(.data, .dots = lazy_dots(...), roll = TRUE, rollends = rollends)
}


fill_na_ <- function(.data, ...,.dots, roll = TRUE ,  rollends = if (roll=="nearest") c(TRUE,TRUE)
  else if (roll>=0) c(FALSE,TRUE)
  else c(TRUE,FALSE), inplace = FALSE){
  	keys <- key(.data)    
	dots <- all_dots(.dots, ...)
	vars <- names(select_vars_(names(.data), dots, exclude = keys))
	if (length(vars) == 0) {
	vars <- lazy_dots(everything())
	}
  if (length(keys)<1){
    stop(".data must be keyed by at least one variable")
  }  
  if (!inplace) .data <- copy(.data) 
  for (col in vars){
    eval(substitute(.data[, (col) := .data[!is.na(x), c(keys, col), with = FALSE ][.data[, c(keys), with = FALSE], value, roll = roll, rollends = rollends]], list(x = as.name(col))))
  }
}




