#' lead and lag.
#'
#' lead and lag are useful for comparing values for date offset by a constant
#' @param x a vector of values
#' @param n a positive integer of length 1, giving the number of positions to lead or lag by. When the package lubridate is loaded, it can be a period when using with along_with (see the lubridate function minutes, hours, days, weeks, months and years)
#' @param order_by override the default ordering to use another vector
#' @param along_with  use this variable as an index instead of the row number
#' @param units Deprecated. Use \code{elapsed_dates}
#' @param default value used for non-existant rows. Defaults to \code{NA}.
#' @param ... Needed for compatibility with lag generic.
#' @examples
#' year <- c(1989, 1991, 1992)
#' value <- c(4.1, 4.5, 3.3)
#' lag(value, 1, order_by = year) # returns value in previous year, like  dplyr::lag
#' lag(value, 1, along_with = year) #  returns value in year - 1
#' lead(value, 1, along_with = year)
#' library(lubridate)
#' date <- mdy(c("04/03/1992", "01/04/1992", "03/15/1992"))
#' value <- c(4.1, 4.5, 3.3, 5.3)
#' datem <- floor_date(date, "month")
#' value_l <- lag(value, units = "month", along_with = datem) 
#' @name lead-lag
NULL

#' @export
#' @rdname lead-lag
lead <- function(x, n = 1L, ...) {
  if (is.null(along_with)){
    dplyr::lead(x = x, n = n, ...)
  }
  lag(x, n=-n, ...)
}


#' @export
#' @rdname lead-lag
lag.default <- function(x, n = 1L, order_by = NULL, along_with = NULL, units = NULL, default = NA, ...) { 
  if (is.null(along_with)){
    dplyr::lag(x = x, n = n, order_by = order_by, default = NA, ...)
  }
  if (!is.null(along_with)) {
    if (!is.null(units)){
      warning(paste0("units is deprecated. Convert to elapsed date with as(",units,")"))
      units <- match.arg(units, c("second", "minute", "hour", "day", "week", "month", "quarter", "year"))
      if (units =="quarter"){
        units <- "month"
        n <- 3 * n
      }
      index <- match(along_with - period(n, units = units), along_with, incomparables = NA)
    } else{
      index <- match(along_with - n, along_with, incomparables = NA)
    }

  out <- x[index]
  if (!is.na(default)) out[which(is.na(index))] <- default
  attributes(out) <- attributes(x)
  out
}

