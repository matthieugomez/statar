#' Elapsed dates (weekly, monthly, quarterly)
#' @param x a vector
#' @examples 
#' library(lubridate)
#' date <- mdy(c("04/03/1992", "01/04/1992", "03/15/1992"))  
#' datem <- as.monthly(date)
#' is.monthly(datem)
#' as.weekly(date)
#' as.quarterly(date)
#' as.character(datem)
#' datem + 1
#' lag(c(1, 2, 3), n = 1, along_with = datem)
#' seq(datem[1], datem[2])
#' as.Date(datem)
#' as.POSIXlt(datem)
#' as.POSIXct(datem)
#' week(datem)
#' @details Weekly, monthly and quarterly dates are stored as integers, representing the number of elapsed calendar periods since 01/01/1970. This allows to use arithmetic on dates other than daily, ie \code{date} + 1 adds one period rather than one day. Moreover, these dates are printed in a way that fits their frequency  (\code{YYY}q\code{q}, \code{YYY}m\code{MM}, \code{YYY}w\code{WW}).
#'
#' Methods to convert from and to Dates or POSIXlt are provided. In particular, you may use lubridate \code{\link{week}} \code{\link{month}} and \code{\link{year}} to extract information from elapsed dates.
#' @name elapsed
#' @aliases quarterly, monthly, weekly
NULL

#' @export
#' @rdname elapsed 
as.quarterly <- function(x){
    date <- as.POSIXlt(x)
    date_origin <- as.POSIXlt(mdy("01/01/1970"))
    out <- 4L*(date$year-date_origin$year) + (date$mon-date_origin$mon) %/% 3
    class(out) <-  "quarterly"
    out
   
}

#' @export
#' @rdname elapsed 
as.monthly <- function(x){
    date <- as.POSIXlt(x)
    date_origin <- as.POSIXlt(mdy("01/01/1970"))
    out <- 12L*(date$year-date_origin$year) + (date$mon-date_origin$mon)
    attributes(out) <- NULL
    class(out) <-  "monthly"
    out
}

#' @export
#' @rdname elapsed 
as.weekly <- function(x){
    date <- as.POSIXlt(x)
    date_origin <- as.POSIXlt(mdy("01/01/1970"))
    out <- 53L*(date$year-date_origin$year) + ((date$yday)%/%7 - date_origin$yday)
    attributes(out) <- NULL
    class(out) <-  "weekly"
    out
}


# as.Date
#' @export
as.Date.quarterly <- function(x, ...){
  attributes(x) <- NULL
  date_origin <- mdy("01/01/1970")
  x <- date_origin+ 3L*months(x)
  as.Date(x, ...)
}

#' @export
as.Date.monthly <- function(x, ...){
  attributes(x) <- NULL
  date_origin <- mdy("01/01/1970")
  x <- date_origin+ months(x)
  as.Date(x, ...)
}

#' @export
as.Date.weekly<- function(x, ...){
  attributes(x) <- NULL
  date_origin <- mdy("01/01/1970")
  x <- date_origin + years(floor(x %/% 53)) + weeks(x-53*floor(x %/% 53))
  as.Date(x, ...)
}


# as.character
#' @export
as.character.quarterly <- function(x, ...){
  paste0(year(x),"q", quarter(x))
}

#' @export
as.character.monthly <- function(x, ...){
  month <- month(x)
  monthc <- as.character(month)
  monthcc <- paste0("0",monthc[which(month<10)])
  monthc[which(month<10)] <- monthcc
  paste0(year(x),"m", monthc)
}

#' @export
as.character.weekly <- function(x, ...){
  week <- week(x)
  weekc <- as.character(week)
  weekcc <- paste0("0", weekc[which(week<10)])
  weekc[which(week<10)] <- weekcc
  paste0(year(x),"w", weekc)
}


# seq
#' @export
seq.quarterly <- function(from,...){
  attributes(from) <- NULL
  out <- seq.int(from,...)
  setattr(out, "class", "quarterly")
  out
}

#' @export
seq.monthly <- function(from,...){
  attributes(from) <- NULL
  out <- seq.int(from,...)
  setattr(out, "class", "monthly")
  out
}

#' @export
seq.weekly <- function(from,...){
  attributes(from) <- NULL
  out <- seq.int(from,...)
  setattr(out, "class", "weekly")
  out
}


#' @export
#' @rdname elapsed 
is.quarterly <- function(x){
  is(x, "quarterly")
}

#' @export
#' @rdname elapsed 
is.monthly <- function(x){
  is(x, "monthly")
}

#' @export
#' @rdname elapsed 
is.weekly <- function(x){
  is(x, "weekly")
}



# Stupid
#' @export
`[.quarterly` <- function(x,..., drop = TRUE){
  out <- NextMethod("[")
  setattr(out, "class", "quarterly")
  out
}

#' @export
`[.monthly` <- function(x,..., drop = TRUE){
  out <- NextMethod("[")
  setattr(out, "class", "monthly")
  out
}


#' @export
`[.weekly` <- function(x,..., drop = TRUE){
  out <- NextMethod("[")
  setattr(out, "class", "weekly")
  out
}


#' @export
as.POSIXlt.quarterly <- function(x, ...){
  as.POSIXlt(as.Date(x), ...)
}

#' @export
as.POSIXlt.monthly <- function(x, ...){
  as.POSIXlt(as.Date(x), ...)
}

#' @export
as.POSIXlt.weekly<- function(x, ...){
  as.POSIXlt(as.Date(x), ...)
}


#' @export
as.POSIXct.quarterly <- function(x, ...){
  as.POSIXct(as.Date(x), ...)
}

#' @export
as.POSIXct.monthly <- function(x, ...){
  as.POSIXct(as.Date(x), ...)
}

#' @export
as.POSIXct.weekly<- function(x, ...){
  as.POSIXct(as.Date(x), ...)
}

#' @export
format.quarterly <- function(x, ...){
  format(as.character(x),...)
}

#' @export
format.monthly <- function(x, ...){
  format(as.character(x),...)
}

#' @export
format.weekly <- function(x, ...){
  format(as.character(x),...)
}

#' @export
print.quarterly <- function(x, ...){
  print(format(x),...)
}

#' @export
print.monthly <- function(x, ...){
  print(format(x),...)
}

#' @export
print.weekly <- function(x, ...){
  print(format(x),...)
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




