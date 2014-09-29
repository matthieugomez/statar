#' Lead and lag.
#'
#' Lead and lag are useful for comparing values offset by a constant (e.g. the
#' previous or next value)
#'
#' @param x a vector of values
#' @param n a postive integer of length 1, giving the number of positions to
#'   lead or lag by
#' @param default value used for non-existant rows. Defaults to \code{NA}.
#' @param order_by override the default ordering to use another vector
#' @param along_with instead of using order_by, along_with computes lag/lead variable based on the variable gien in along_with - n
#' @param units when variable in along_with is a date, computes lag based on "day", "week", "month", "quarter" or "year". Internally, dates are converted to their largest unit not greater than the corresponding date.
#' @param ... Needed for compatibility with lag generic.
#' @examples
#' lead(1:10, 1)
#' lead(1:10, 2)
#'
#' lag(1:10, 1)
#' lead(1:10, 1)
#'
#' x <- runif(5)
#' cbind(ahead = lead(x), x, behind = lag(x))
#'
#' # Use order_by if data not already ordered
#' df <- data.frame(year = 2000:2005, value = (0:5) ^ 2)
#' scrambled <- df[sample(nrow(df)), ]
#' right <- mutate(scrambled, prev = lag(value, order_by = year))
#' arrange(right, year)

#' Use along_with if unbalanced panel data
#' library(lubridate)
#' date  <-  dmy(c("01031992","03041992","05051992","21081992"))
#' value <-  c(4.1,4.5,3.3,5.3)
#' value_lag <- lead(value,1,along_with = date_date, units = "month")
#' @name lead-lag
NULL

#' @export
#' @rdname lead-lag
lead <- function(x, n = 1L, default = NA, order_by = NULL, units = NULL, along_with = NULL,  ...) {
  if (!is.null(order_by)) {
    if (!is.null(along_with)) stop("order_by and along_with cannot be specified together")
    if (!is.null(units)) stop("order_by and units cannot be specified together")
    return(with_order(order_by, lead, x, n = n, default = default))
  }
  if (!is.null(units)) {
    if (is.null(along_with)) stop("units cannot be used without order_by")
    unitsc <-match.arg(units,c("day","week","month","quarter","year"))
    date_origin <- as.Date('0001-01-01')
    if (unitsc=="day"){
      return(lead(x = x, n = n, default = default, along_with = along))
    }
    if (unitsc == "week"){
      along_with_elapsed <- as.period(along_with-date_origin)  %/% weeks(1)
      return(lead(x = x, n = n, default = default, along_with = along_with_elapsed))
    }  
    else if (unitsc == "month"){
      along_with_elapsed <- as.period(along_with-date_origin)  %/% months(1)
      return(lead(x = x, n = n, default = default, along_with = along_with_elapsed))
    } else if (unitsc == "quarter"){
      along_with_elapsed <- as.period(along_with-date_origin)  %/% 3*months(1)
      return(lead(x = x, n = n, default = default, along_with = along_with_elapsed))
    } else if (unitsc == "year"){
      along_with_elapsed <- as.period(along_with-date_origin)  %/% years(1)
      return(lead(x = x, n = n, default = default, along_with = along_with_elapsed))
    }
  }

  if (n == 0) return(x)
  if (n < 0 || length(n) > 1) stop("n must be a single positive integer")

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
lag.default <- function(x, n = 1L, default = NA, order_by = NULL, units = NULL, along_with = NULL, ...) {
  if (!is.null(order_by)) {
    if (!is.null(along_with)) stop("order_by and along_with cannot be specified together")
    if (!is.null(units)) stop("order_by and units cannot be specified together")
    return(with_order(order_by, lead, x, n = n, default = default))
  }
  if (!is.null(units)) {
    if (is.null(along_with)) stop("units cannot be used without order_by")
    unitsc <-match.arg(units,c("day","week","month","quarter","year"))
    date_origin <- as.Date('0001-01-01')
    if (unitsc=="day"){
      return(lead(x = x, n = n, default = default, along_with = along))
    }
    if (unitsc == "week"){
      along_with_elapsed <- as.period(along_with-date_origin)  %/% weeks(1)
      return(lead(x = x, n = n, default = default, along_with = along_with_elapsed))
    }  
    else if (unitsc == "month"){
      along_with_elapsed <- as.period(along_with-date_origin)  %/% months(1)
      return(lead(x = x, n = n, default = default, along_with = along_with_elapsed))
    } else if (unitsc == "quarter"){
      along_with_elapsed <- as.period(along_with-date_origin)  %/% 3*months(1)
      return(lead(x = x, n = n, default = default, along_with = along_with_elapsed))
    } else if (unitsc == "year"){
      along_with_elapsed <- as.period(along_with-date_origin)  %/% years(1)
      return(lead(x = x, n = n, default = default, along_with = along_with_elapsed))
    }
  }

  if (n == 0) return(x)
  if (n < 0 || length(n) > 1) stop("n must be a single positive integer")

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