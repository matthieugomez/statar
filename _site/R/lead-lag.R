#' lead and lag.
#'
#' lead and lag are useful for comparing values for date offset by a constant
#' @param x a vector of values
#' @param n a positive integer of length 1, giving the number of positions to lead or lag by. When the package lubridate is loaded, it can be a period when using with along_with (see the lubridate function minutes, hours, days, weeks, months and years)
#' @param order_by override the default ordering to use another vector
#' @param along_with  compute lag with respect to this vector instead of previous row
#' @param units A character when along_with is a date (one of "second",  "minute", "hour", "day", "week", "month", "quarter", "year").  
#' @param default value used for non-existant rows. Defaults to \code{NA}.
#' @param ... Needed for compatibility with lag generic.
#' @examples
#' year <- c(1992, 1989, 1991)
#' value <- c(4.1, 4.5, 3.3)
#' lag(value, 1, order_by = year) # returns value in previous year, like  dplyr::lag
#' lag(value, 1, along_with = year) #  returns value in year - 1
#' 
#' library(lubridate)
#' date <- mdy(c("04/03/1992", "01/04/1992", "03/15/1992"))
#' value <- c(4.1, 4.5, 3.3, 5.3)
#' datem <- floor_date(date, "month")
#' value_l <- lag(value, units = "month", along_with = datem) 
#' @name lead-lag
NULL

#' @export
#' @rdname lead-lag
lead <- function(x, n = 1L, order_by = NULL, units = NULL, along_with = NULL, default = NA,  ...) {
  if (!is.numeric(n) | (length(n)>1)) stop("n must be a numeric of length one")
  if (!is.null(order_by)) {
    if (!is.null(along_with))  stop("order_by cannot be used with along_with")
    if (!is.null(units))  stop("order_by cannot be used with units")
    return(with_order(order_by, lead, x, n = n, default = default))
  }
  if (!is.null(along_with)) {
    if (!is.null(units)){
      units <- match.arg(units, c("second", "minute", "hour", "day", "week", "month", "quarter", "year"))
      if ( units == "quarter"){
        units <- "month"
        n <- 3 * n
      }
      index <- match(along_with + period(n, units = units), along_with, incomparables = NA)
    } else{
      index <- match(along_with + n, along_with, incomparables = NA)
    }
    out <- x[index]
    if (!is.na(default)) out[which(is.na(index))] <- default
  } else{
    if (!is.null(units)) stop("Units is specified but along_with is not specified")
    xlen <- length(x)
    n <- pmin(n, xlen)
    out <- c(x[-seq_len(n)], rep(default, n))
  }
  attributes(out) <- attributes(x)
  out
}


#' @export
#' @rdname lead-lag
lag.default <- function(x, n = 1L, order_by = NULL, units = NULL, along_with = NULL, default = NA, ...) { 
  if (!is.numeric(n) | (length(n)>1)) stop("n must be a numeric of length one")
  if (!is.null(order_by)) {
    if (!is.null(along_with))  stop("order_by cannot be used with along_with")
    if (!is.null(units))  stop("order_by cannot be used with units")
    return(with_order(order_by, lag, x, n = n, default = default))
 }

  if (!is.null(along_with)) {
    if (!is.null(units)){
      units <- match.arg(units, c("second", "minute", "hour", "day", "week", "month", "quarter", "year"))
      if (units =="quarter"){
        units <- "month"
        n <- 3 * n
      }
      index <- match(along_with - period(n, units = units), along_with, incomparables = NA)
    } else{
      index <- match(along_with + n, along_with, incomparables = NA)
    }

    out <- x[index]
    if (!is.na(default)) out[which(is.na(index))] <- default
  } else{
    if (!is.null(units)) stop("Units is specified but along_with is not specified")
    xlen <- length(x)
    n <- pmin(n, xlen)
    out <- c(rep(default, n), x[seq_len(xlen - n)])
  }
  attributes(out) <- attributes(x)
  out
}

