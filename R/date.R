#' Elapsed dates (weekly, monthly, quarterly)
#'
#' @examples 
#' date <- mdy(c("04/03/1992", "01/04/1992", "03/15/1992"))  
#' datem <-  as.monthly(date)
#' # Print depends on period
#' datem
#' as.character(datem)
#' # Internally stocked as inter, which allows things like:
#' datem+1
#' lag(c(1, 2, 3), n = 1, along_with = datem)
#' # Behaves as a date for lubridate operation
#' week(datem)
#' # Convert to usual date format if needed
#' as.POSIXlt(datem)
#' as.POSIXct(datem)
#' as.Date(datem)
#' @details Weekly, monthly and quarterly dates are stored as the number of elapsed calendar periods since 01/01/1970. This allows to use arithmetic on dates other than daily. Moreover, these dates are printed in a way that fits their frequency  (\code{yyyy}q\code{q}, \code{yyyy}m\code{MM}, \code{yyyy}m\code{WW}).
#'
#' Methods to convert from and to Dates or POSIXlt are provided. In particular, you may use lubridate \code{\link{week}} \code{\link{month}} and \code{\link{year}} to extract informations from elapsed dates.
#' @name elapsed
#' @aliases quarterly, monthly, weekly
NULL

#' @export
#' @rdname elapsed 
as.quarterly <- function(date){
    date <- as.POSIXlt(date)
    date_origin <- as.POSIXlt(mdy("01/01/1970"))
    out <- 4L*(date$year-date_origin$year) + (date$mon-date_origin$mon) %/% 3
    class(out) <-  "quarterly"
    out
   
}

#' @export
#' @rdname elapsed 
as.monthly <- function(date){
    date <- as.POSIXlt(date)
    date_origin <- as.POSIXlt(mdy("01/01/1970"))
    out <- 12L*(date$year-date_origin$year) + (date$mon-date_origin$mon)
    attributes(out) <- NULL
    class(out) <-  "monthly"
    out
}

#' @export
#' @rdname elapsed 
as.weekly <- function(date){
    date <- as.POSIXlt(date)
    date_origin <- as.POSIXlt(mdy("01/01/1970"))
    out <- 53L*(date$year-date_origin$year) + ((date$yday)%/%7 - date_origin$yday)
    attributes(out) <- NULL
    class(out) <-  "weekly"
    out
}


# as.Date
#' @export
as.Date.quarterly <- function(date, ...){
  attributes(date) <- NULL
  date_origin <- mdy("01/01/1970")
  date <- date_origin+ 3L*months(date)
  as.Date(date, ...)
}

#' @export
as.Date.monthly <- function(date, ...){
  attributes(date) <- NULL
  date_origin <- mdy("01/01/1970")
  date <- date_origin+ months(date)
  as.Date(date, ...)
}

#' @export
as.Date.weekly<- function(date, ...){
  attributes(date) <- NULL
  date_origin <- mdy("01/01/1970")
  date <- date_origin + years(floor(date %/% 53)) + weeks(date-53*floor(date %/% 53))
  as.Date(date, ...)
}


# as.character
#' @export
as.character.quarterly <- function(date, ...){
  paste0(year(date),"q", quarter(date))
}

#' @export
as.character.monthly <- function(date, ...){
  month <- month(date)
  monthc <- as.character(month)
  monthcc <- paste0("0",monthc[which(month<10)])
  monthc[which(month<10)] <- monthcc
  paste0(year(date),"m", monthc)
}

#' @export
as.character.weekly <- function(date, ...){
  week <- week(date)
  weekc <- as.character(week)
  weekcc <- paste0("0", weekc[which(week<10)])
  weekc[which(week<10)] <- weekcc
  paste0(year(date),"w", weekc)
}



# Stupid

#' @export
as.POSIXlt.quarterly <- function(date, ...){
  as.POSIXlt(as.Date(date), ...)
}

#' @export
as.POSIXlt.monthly <- function(date, ...){
  as.POSIXlt(as.Date(date), ...)
}

#' @export
as.POSIXlt.weekly<- function(date, ...){
  as.POSIXlt(as.Date(date), ...)
}


#' @export
as.POSIXct.quarterly <- function(date, ...){
  as.POSIXct(as.Date(date), ...)
}

#' @export
as.POSIXct.monthly <- function(date, ...){
  as.POSIXct(as.Date(date), ...)
}

#' @export
as.POSIXct.weekly<- function(date, ...){
  as.POSIXct(as.Date(date), ...)
}



#' @export
format.quarterly <- function(date, ...){
  format(as.character(date))
}

#' @export
format.monthly <- function(date, ...){
  format(as.character(date))
}

#' @export
format.weekly <- function(date, ...){
  format(as.character(date))
}

#' @export
print.quarterly <- function(date){
  print(format(date))
}

#' @export
print.monthly <- function(date){
  print(format(date))
}

#' @export
print.weekly <- function(date){
  print(format(date))
}

# stupid stuff
#' @export
as.data.frame.monthly <- function(...){
    as.data.frame.vector(...)
}

#' @export
as.data.frame.quarterly <- function(...){
    as.data.frame.vector(...)
}

#' @export
as.data.frame.weekly <- function(...){
    as.data.frame.vector(...)
}


