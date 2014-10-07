#' lead and lag.
#'
#' lead and lag are useful for comparing values for date offset by a constant
#' @param x a vector of values
#' @param n a positive integer of length 1, giving the number of positions to lead or lag by. When the package lubridate is loaded, it can be a period when using with along_with (see the lubridate function minutes, hours, days, weeks, months and years)
#' @param order_by override the default ordering to use another vector
#' @param along_with  compute lag with respect to this vector instead of previous row
#' @param default value used for non-existant rows. Defaults to \code{NA}.
#' @examples
#' # along_with
#' DT <- data.table(
#'  id    = c(1, 1, 1, 1, 1, 2, 2),
#'  date  = c(1992, 1989, 1991, 1990, 1994, 1992, 1991),
#'  value = c(4.1, 4.5, 3.3, 5.3, 3.0, 3.2, 5.2)
#' )
#' DT %>% group_by(id) %>% mutate(lag(value, 1, order_by = date)) # wrong
#' DT %>% group_by(id) %>% mutate(lag(value, 1, along_with = date)) # right
#' # units
#'library(lubridate)
#'df <- data.frame(
#'     id = c("1", "1", "1", "1"),
#'   date = mdy(c("03/01/1992", "04/03/1992", "07/15/1992", "08/21/1992"), "%m/%d/%Y"),
#'  value = c(4.1, 4.5, 3.3, 5.3)
#'   )
#' df <- df %>% mutate(datem = floor_date(date, "month"))
#' DT %>% group_by(id) %>% mutate(lag(value, months(1), along_with = datem)) 
#' @name lead-lag
NULL

#' @export
#' @rdname lead-lag
lead <- function(x, n = 1L, order_by = NULL, along_with = NULL, default = NA, ...) {
  if (!inherits(n,"Period")){
    if (n == 0) return(x)
    if (n < 0 || length(n) > 1) stop("n must be a single positive integer")
  }

  if (!is.null(order_by)) {
    if (!is.null(along_with))  stop("order_by cannot be used with along_with")
    return(with_order(order_by, lead, x, n = n, default = default))
  }

  if (!is.null(along_with)) {
    index <- match(along_with + n, along_with, incomparable = NA)
    out <- x[index]
    if (!is.na(default)) out[which(is.na(index))] <- default
  } else{
    xlen <- length(x)
    n <- pmin(n, xlen)
    out <- c(x[-seq_len(n)], rep(default, n))
  }
  attributes(out) <- attributes(x)
  out
}



#' @export
#' @rdname lead-lag
lag.default <- function(x, n = 1L, order_by = NULL, along_with = NULL, default = NA, ...) { 
  if (!inherits(n,"Period")){
    if (n == 0) return(x)
    if (n < 0 || length(n) > 1) stop("n must be a single positive integer")
  }

  if (!is.null(order_by)) {
    if (!is.null(along_with))  stop("order_by cannot be used with along_with")
    return(with_order(order_by, lead, x, n = n, default = default))
 }

  if (!is.null(along_with)) {
    index <- match(along_with - n, along_with, incomparable = NA)
    out <- x[index]
    if (!is.na(default)) out[which(is.na(index))] <- default
  } else{
    xlen <- length(x)
    n <- pmin(n, xlen)
    out <- c(rep(default, n), x[seq_len(xlen - n)])
  }
  attributes(out) <- attributes(x)
  out
}


