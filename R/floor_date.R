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