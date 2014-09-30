#' Lead_along and lag_along.
#'
#' lead_along and lag_along are useful for comparing values for date offset by a constant
#' @param x a vector of values
#' @param n a postive integer of length 1, giving the number of positions to lead or lag by
#' @param time specifes the time variable
#' @param units computes lag based on "day", "week", "month", "quarter" or "year" periods. Should be used when \code{time} is a date. 
#' @param default value used for non-existant rows. Defaults to \code{NA}.
#' @examples
#' # Unbalanced panel
#' DT <- data.table(
#'  id    = c(1, 1, 1, 1, 1, 2, 2),
#'  date  = c(1992, 1989, 1991, 1990, 1994, 1992, 1991),
#'  value = c(4.1, 4.5, 3.3, 5.3, 3.0, 3.2, 5.2)
#' )
#' DT %>% group_by(id) %>% mutate(lag(value, 1, order_by = date)) # wrong
#' DT %>% group_by(id) %>% mutate(lag_along(value, 1, time = date)) # right

#' # Units
#' DT[, date := as.Date(c("01/03/1992", "03/04/1992", "05/05/1992", "21/08/1992"), "%m/%d/%y")]
#' DT %>% group_by(id) %>% mutate(lag_along(value, 1, units = "month", time = date)) 
#' @name lead_along-lag_along
NULL

#' @export
#' @rdname lead_along-lag_along
lead_along <- function(x, n = 1L, time, units = NULL, default = NA) {
  if (!is.null(units)) {
    if (is.null(time)) stop("units cannot be used without order_by")
    unitsc <-match.arg(units,c("day","week","month","quarter","year"))
    time_origin <- as.time('0001-01-01')
    if (unitsc=="day"){
      return(lead_along(x = x, n = n, default = default, time = along))
    }
    if (unitsc == "week"){
      time_elapsed <- as.period(time-time_origin)  %/% weeks(1)
      return(lead_along(x = x, n = n, default = default, time = time_elapsed))
    }  
    else if (unitsc == "month"){
      time_elapsed <- as.period(time-time_origin)  %/% months(1)
      return(lead_along(x = x, n = n, default = default, time = time_elapsed))
    } else if (unitsc == "quarter"){
      time_elapsed <- as.period(time-time_origin)  %/% 3*months(1)
      return(lead_along(x = x, n = n, default = default, time = time_elapsed))
    } else if (unitsc == "year"){
      time_elapsed <- as.period(time-time_origin)  %/% years(1)
      return(lead_along(x = x, n = n, default = default, time = time_elapsed))
    }
  }

  if (n == 0) return(x)
  if (n < 0 || length(n) > 1) stop("n must be a single positive integer")
  index <- match(time + n, time, incomparable = NA)
  out <- x[index]
  if (!is.na(default)) out[which(is.na(index))] <- default
  attributes(out) <- attributes(x)
  out
}

#' @export
#' @rdname lead_along-lag_along
lag_along <- function(x, n = 1L, time, units = NULL, default = NA) {
    if (!is.null(units)) {
      if (is.null(time)) stop("units cannot be used without order_by")
      unitsc <-match.arg(units,c("day","week","month","quarter","year"))
      time_origin <- as.time('0001-01-01')
      if (unitsc=="day"){
        return(lag_along(x = x, n = n, default = default, time = along))
      }
      if (unitsc == "week"){
        time_elapsed <- as.period(time-time_origin)  %/% weeks(1)
        return(lag_along(x = x, n = n, default = default, time = time_elapsed))
      }  
      else if (unitsc == "month"){
        time_elapsed <- as.period(time-time_origin)  %/% months(1)
        return(lag_along(x = x, n = n, default = default, time = time_elapsed))
      } else if (unitsc == "quarter"){
        time_elapsed <- as.period(time-time_origin)  %/% 3*months(1)
        return(lag_along(x = x, n = n, default = default, time = time_elapsed))
      } else if (unitsc == "year"){
        time_elapsed <- as.period(time-time_origin)  %/% years(1)
        return(lag_along(x = x, n = n, default = default, time = time_elapsed))
      }
    }

    if (n == 0) return(x)
    if (n < 0 || length(n) > 1) stop("n must be a single positive integer")
    index <- match(time - n, time, incomparable = NA)
    out <- x[index]
    if (!is.na(default)) out[which(is.na(index))] <- default
    attributes(out) <- attributes(x)
    out
  }