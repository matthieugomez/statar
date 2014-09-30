#' floor_date 
#' Round date-times down.
#'
#' floor_date takes a date-time object and rounds it down to the nearest integer 
#' value of the specified time unit. Users can specify whether to round down to 
#' the nearest second, minute, hour, day, week, month, quarter or year.
#'
#' @export floor_date
#' @param x a vector of date-time objects 
#' @param unit a character string specifying the time unit to be rounded to. Should be one of 
#'   "second","minute","hour","day", "week", "month", or "year."
#' @return x with the appropriate units floored
#' @seealso \code{\link{ceiling_date}}, \code{\link{round_date}}
#' @keywords manip chron
#' @examples
#' x <- as.POSIXct("2009-08-03 12:01:59.23")
#' floor_date(x, "second")
#' # "2009-08-03 12:01:59 CDT"
#' floor_date(x, "minute")
#' # "2009-08-03 12:01:00 CDT"
#' floor_date(x, "hour")
#' # "2009-08-03 12:00:00 CDT"
#' floor_date(x, "day")
#' # "2009-08-03 CDT"
#' floor_date(x, "week")
#' # "2009-08-02 CDT"
#' floor_date(x, "month")
#' # "2009-08-01 CDT"
#' floor_date(x, "quarter")
#' # "2009-07-01 CDT"
#' floor_date(x, "year")
#' # "2009-01-01 CST"

floor_date <- function (x, unit = c("second", "minute", "hour", "day", "week",  
    "month","quarter", "year")) {
    unit <- match.arg(unit)   
    unit_temp <- unit
    if (unit=="quarter")  unit_temp <-  "month" 
    new <- switch(unit_temp,
        second = update(x, seconds = floor(second(x))),
        minute = update(x, seconds = 0),
          hour = update(x, minutes = 0, seconds = 0),
           day = update(x, hours = 0, minutes = 0, seconds = 0),
          week = update(x, wdays = 1, hours = 0, minutes = 0, seconds = 0),
         month = update(x, mdays = 1, hours = 0, minutes = 0, seconds = 0),
          year = update(x, ydays = 1, hours = 0, minutes = 0, seconds = 0)
        )
    if (unit == "quarter"){
      modulo <- (month(new)-1) %% 3 
      new <- ifelse(modulo==1, new - months(1), ifelse(modulo==2, new - months(2),new))
      attributes(new) <- attributes(x)
    }
    new
}