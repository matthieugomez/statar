#' Elapsed dates (monthly, quarterly)
#' @param x a vector
#' @examples 
#' library(lubridate)
#' date <- mdy(c("04/03/1992", "01/04/1992", "03/15/1992"))  
#' datem <- as.monthly(date)
#' is.monthly(datem)
#' as.quarterly(date)
#' as.character(datem)
#' datem + 1
#' lag(c(1, 2, 3), n = 1, along_with = datem)
#' seq(datem[1], datem[2])
#' as.Date(datem)
#' as.POSIXlt(datem)
#' as.POSIXct(datem)
#' week(datem)
#' @details Weekly, monthly and quarterly dates are stored as integers, representing the number of elapsed calendar periods since 01/01/1970.  As \code{yearmonth} and \code{yearqtr} the package \code{zoo}, these dates are printed in a way that fits their frequency  (\code{YYY}q\code{q}, \code{YYY}m\code{MM}). The only difference is that, \code{monthly}, and \code{quarterly} are integers, which removes issues due to floating points (particularly important when merging). This also allows to use arithmetic on perios, ie \code{date} + 1 adds one period rather than one day.
#'
#' Methods to convert from and to Dates or POSIXlt are provided. In particular, you may use lubridate \code{\link{week}} \code{\link{month}} and \code{\link{year}} to extract information from elapsed dates.
#' @name elapsed
#' @aliases quarterly, monthly
NULL

#' @export
#' @rdname elapsed 
as.quarterly <- function(x) {
  if (length(class(x))==1 && class(x) == "numeric"){
    out <- x
    } else if (length(class(x))==1 && class(x) == "character"){
    date <- str_match(x,"(.*)q(.*)")
    out <- 4L*(as.integer(date[,2])-70) + (as.integer(date[,3]) -1)
  } else{
    date <- as.POSIXlt(x)
    out <- 4L*(date$year-70) + date$mon %/% 3
  }
  class(out) <-  "quarterly"
  out
}

#' @export
#' @rdname elapsed 
as.monthly <- function(x) {
  if (length(class(x))==1 && class(x) == "numeric"){
    out <- x
  } else if (length(class(x))==1 && class(x) == "character"){
   date <- str_match(x,"(.*)m(.*)")
   out <- 12L*(as.integer(date[,2])-70) + (as.integer(date[,3]) -1)
  } else{
    date <- as.POSIXlt(x)
    out <- 12L*(date$year-70) + (date$mon)
  } 
  class(out) <-  "monthly"
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
#' @rdname elapsed 
is.quarterly <- function(x){
  is(x, "quarterly")
}

#' @export
#' @rdname elapsed 
is.monthly <- function(x){
  is(x, "monthly")
}




# Stupid


#' @export
as.POSIXlt.quarterly <- function(x, ...){
  as.POSIXlt(as.Date(x), ...)
}

#' @export
as.POSIXlt.monthly <- function(x, ...){
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
format.quarterly <- function(x, ...){
  format(as.character(x),...)
}

#' @export
format.monthly <- function(x, ...){
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

# stupid stuff
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
as.data.frame.monthly <- function(...){
    as.data.frame.vector(...)
}

#' @export
as.data.frame.quarterly <- function(...){
    as.data.frame.vector(...)
}






