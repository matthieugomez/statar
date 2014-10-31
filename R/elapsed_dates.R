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
#' @details Monthly and quarterly dates are stored as integers, representing the number of elapsed calendar periods since 01/01/1970.  As \code{yearmonth} and \code{yearqtr} the package \code{zoo}, these dates are printed in a way that fits their frequency  (\code{YYY}q\code{q}, \code{YYY}m\code{MM}). The only difference is that, \code{monthly}, and \code{quarterly} are integers, which removes issues due to floating points (particularly important when merging). This also allows to use arithmetic on perios, ie \code{date} + 1 adds one period rather than one day.
#'
#' Methods to convert from and to Dates or POSIXlt are provided. In particular, you may use lubridate \code{\link{week}} \code{\link{month}} and \code{\link{year}} to extract information from elapsed dates.
#' @name elapsed
#' @aliases quarterly, monthly
NULL

#' @export
#' @rdname elapsed 
as.quarterly <- function(x) {
  UseMethod("as.quarterly")
}
#' @export
as.quarterly.default <- function(x, ...) as.quarterly(as.POSIXlt(x, ...))
#' @export
as.quarterly.POSIXlt <- function(x, ...) {
    structure(4L*(x$year-70L) + x$mon %/% 3, class=c("quarterly"))
}  
#' @export
as.quarterly.numeric <- function(x,...){
  class(x) <- "quarterly"
  x
}

# test
#' @export
#' @rdname elapsed 
is.quarterly <- function(x){
  is(x, "quarterly")
}

# Convert to 
#' @export
as.Date.quarterly <- function(x, ...){
  attributes(x) <- NULL
  date_origin <- as.Date("1970-01-01")
  x <- date_origin+ 3L*months(x)
  as.Date(x, ...)
}
#' @export
as.POSIXct.quarterly <- function(x, ...){as.POSIXct(as.Date(x,...))}
#' @export
as.POSIXlt.quarterly <- function(x, ...){as.POSIXlt(as.Date(x,...))}


# Print
#' @export
as.character.quarterly <- function(x, ...){
  paste0(year(x),"q", quarter(x))
}
#' @export
format.quarterly <- function(x, ...){
  format(as.character(x),...)
}
#' @export
print.quarterly <- function(x, ...){
  print(format(x),...)
}
#' @export
as.data.frame.quarterly <- function(...){
  as.data.frame.vector(...)
}
#' @export
mean.quarterly <- function(x, ...) {as.quarterly(NextMethod())}
#' @export
cut.quarterly <- function(x, ...) {as.quarterly(NextMethod())}
#' @export
seq.quarterly <- function(x, ...) {as.quarterly(NextMethod())}
#' @export
c.quarterly <- function(x, ...) {as.quarterly(NextMethod())}
#' @export
rep.quarterly <- function(x, ...) {as.quarterly(NextMethod())}
#' @export
split.quarterly <- function(x, ...) {as.quarterly(NextMethod())}
#' @export
as.list.quarterly <- function(x, ...) {as.quarterly(NextMethod())}
#' @export
unique.quarterly <- function(x, ...) {as.quarterly(NextMethod())}
#' @export
`[.quarterly` <- function(x, ...) {as.quarterly(NextMethod())}







#' @export
#' @rdname elapsed 
as.monthly <- function(x) {
  UseMethod("as.monthly")
}
#' @export
as.monthly.default <- function(x, ...) as.monthly(as.POSIXlt(x, ...))
#' @export
as.monthly.POSIXlt <- function(x, ...) {
  structure(12L*(x$year-70L) + x$mon, class=c("monthly"))
}      
#' @export
as.monthly.numeric <- function(x,...){
  class(x) <- "monthly"
  x
}

# Test
#' @export
#' @rdname elapsed 
is.monthly <- function(x){
  is(x, "monthly")
}

# Convert to 
#' @export
as.Date.monthly <- function(x, ...){
  attributes(x) <- NULL
  date_origin <- as.Date("1970-01-01")
  x <- date_origin + months(x)
  as.Date(x, ...)
}
#' @export
as.POSIXct.monthly <- function(x, ...){as.POSIXct(as.Date(x,...))}
#' @export
as.POSIXlt.monthly <- function(x, ...){as.POSIXlt(as.Date(x,...))}


# print
#' @export
as.character.monthly <- function(x, ...){
  paste0(year(x),"m", month(x))
}
#' @export
format.monthly <- function(x, ...){
  format(as.character(x),...)
}
#' @export
print.monthly <- function(x, ...){
  print(format(x),...)
}
#' @export
as.data.frame.monthly <- function(...){
  as.data.frame.vector(...)
}
#' @export
mean.monthly <- function(x, ...) {as.monthly(NextMethod())}
#' @export
cut.monthly <- function(x, ...) {as.monthly(NextMethod())}
#' @export
seq.monthly <- function(x, ...) {as.monthly(NextMethod())}
#' @export
c.monthly <- function(x, ...) {as.monthly(NextMethod())}
#' @export
rep.monthly <- function(x, ...) {as.monthly(NextMethod())}
#' @export
split.monthly <- function(x, ...) {as.monthly(NextMethod())}
#' @export
as.list.monthly <- function(x, ...) {as.monthly(NextMethod())}
#' @export
unique.monthly <- function(x, ...) {as.monthly(NextMethod())}
#' @export
`[.monthly` <- function(x, ...) {as.monthly(NextMethod())}










