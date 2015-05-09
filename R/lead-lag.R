#' lead and lag with respect to a time variable
#'
#' @param x a vector of values
#' @param n a positive integer of length 1, giving the number of positions to lead or lag by. When the package lubridate is loaded, it can be a period when using with along_with (see the lubridate function minutes, hours, days, weeks, months and years)
#' @param order_by override the default ordering to use another vector
#' @param along_with  use this variable to lag with respect to \code{n} \code{along_with} rather than \code{n} row 
#' @param default value used for non-existant rows. Defaults to \code{NA}.
#' @param units Deprecated. Use \code{elapsed_dates}
#' @param ... Needed for compatibility with lag generic.
#' @examples
#' year <- c(1989, 1991, 1992)
#' value <- c(4.1, 4.5, 3.3)
#' lag(value, 1, order_by = year) # returns value in previous year, like  dplyr::lag
#' lag(value, 1, along_with = year) #  returns value in year - 1
#' lead(value, 1, along_with = year)
#' library(lubridate)
#' date <- mdy(c("01/04/1992", "03/15/1992", "04/03/1992"))
#' value <- c(4.1, 4.5, 3.3)
#' datem <- as.monthly(date)
#' lag(value, along_with = datem) 
#' @name lead-lag
NULL


#' @export
#' @rdname lead-lag
lead <- function(x, n = 1L, order_by = NULL, along_with = NULL, units = NULL, default = NA,...) {
  if (!is.numeric(n) | (length(n)>1)) stop("n must be a numeric of length one")
   if (!is.null(order_by)) {
     if (!is.null(along_with))  stop("order_by cannot be used with along_with")
     if (!is.null(units))  stop("order_by cannot be used with units")
     return(with_order(order_by, lead, x, n = n, default = default))
   }
  if (!is.null(along_with)) {
     if (!is.null(units)){
      warning(paste0("units is deprecated. Convert to elapsed date with as(",units,")"))
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
lag.default <- function(x, n = 1L, order_by = NULL, along_with = NULL, units = NULL, default = NA, ...) { 
  if (!is.numeric(n) | (length(n)>1)) stop("n must be a numeric of length one")
  if (!is.null(order_by)) {
    if (!is.null(along_with))  stop("order_by cannot be used with along_with")
    if (!is.null(units))  stop("order_by cannot be used with units")
    return(with_order(order_by, lag, x, n = n, default = default))
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
  } else{
    if (!is.null(units)) stop("Units is specified but along_with is not specified")
    xlen <- length(x)
    n <- pmin(n, xlen)
    out <- c(rep(default, n), x[seq_len(xlen - n)])
  }
  attributes(out) <- attributes(x)
  out
}