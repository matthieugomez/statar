#' Elapsed dates (monthly, quarterly)
#' @param x a vector
#' @examples 
#' library(lubridate)
#' library(dplyr)
#' date <- mdy(c("04/03/1992", "01/04/1992", "03/15/1992"))  
#' datem <- as.monthly(date)
#' is.monthly(datem)
#' as.quarterly(date)
#' as.character(datem)
#' datem + 1
#' df <- tibble(datem)
#' # filter(df, month(datem) == 1)
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
#' @method as.quarterly default
as.quarterly.default <- function(x, ...) as.quarterly(as.POSIXlt(x, ...))

#' @export
#' @method as.quarterly POSIXlt
as.quarterly.POSIXlt <- function(x, ...) {
    structure(4L*(x$year-70L) + x$mon %/% 3, class=c("quarterly"))
}  
#' @export
#' @method as.quarterly numeric
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
#' @method as.Date quarterly
as.Date.quarterly <- function(x, ...){
  attributes(x) <- NULL
  date_origin <- as.Date("1970-01-01")
  x <- date_origin+ 3L*months(x)
  as.Date(x, ...)
}

#' @export
#' @method as.POSIXct quarterly
as.POSIXct.quarterly <- function(x, ...){as.POSIXct(as.Date(x,...))}

#' @export
#' @method as.POSIXlt quarterly
as.POSIXlt.quarterly <- function(x, ...){as.POSIXlt(as.Date(x,...))}


# Print
#' @export
#' @method as.character quarterly
as.character.quarterly <- function(x, ...){
  paste0(as.POSIXlt(x)$year + 1900L,"q", as.POSIXlt(x)$mon%/%3L + 1L)
}

#' @export
#' @method format quarterly
format.quarterly <- function(x, ...){
  format(as.character(x),...)
}

#' @export
#' @method print quarterly
print.quarterly <- function(x, ...){
  print(format(x),...)
}

#' @export
#' @method as.data.frame quarterly
as.data.frame.quarterly <- function(...){
  as.data.frame.vector(...)
}

#' @export
#' @method mean quarterly
mean.quarterly <- function(x, ...) {as.quarterly(NextMethod())}

#' @export
#' @method cut quarterly
cut.quarterly <- function(x, ...) {as.quarterly(NextMethod())}

#' @export
#' @method seq quarterly
seq.quarterly <- function(x, ...) {as.quarterly(NextMethod())}

#' @export
#' @method c quarterly
c.quarterly <- function(x, ...) {as.quarterly(NextMethod())}

#' @export
#' @method rep quarterly
rep.quarterly <- function(x, ...) {as.quarterly(NextMethod())}

#' @export
#' @method split quarterly
split.quarterly <- function(x, ...) {as.quarterly(NextMethod())}

#' @export
#' @method as.list quarterly
as.list.quarterly <- function(x, ...) {as.quarterly(NextMethod())}

#' @export
#' @method unique quarterly
unique.quarterly <- function(x, ...) {as.quarterly(NextMethod())}

#' @export
#' @method [ quarterly
`[.quarterly` <- function(x, ...) {as.quarterly(NextMethod())}







#' @export
#' @rdname elapsed 
as.monthly <- function(x) {
  UseMethod("as.monthly")
}

#' @export
#' @method as.monthly default
as.monthly.default <- function(x, ...) as.monthly(as.POSIXlt(x, ...))

#' @export
#' @method as.monthly POSIXlt
as.monthly.POSIXlt <- function(x, ...) {
  structure(12L*(x$year-70L) + x$mon, class=c("monthly"))
}      
#' @export
#' @method as.monthly numeric
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
#' @method as.Date monthly
as.Date.monthly <- function(x, ...){
  attributes(x) <- NULL
  date_origin <- as.Date("1970-01-01")
  x <- date_origin + months(x)
  as.Date(x, ...)
}

#' @export
#' @method as.POSIXct monthly
as.POSIXct.monthly <- function(x, ...){as.POSIXct(as.Date(x,...))}

#' @export
#' @method as.POSIXlt monthly
as.POSIXlt.monthly <- function(x, ...){as.POSIXlt(as.Date(x,...))}


# print
#' @export
#' @method as.character monthly
as.character.monthly <- function(x, ...){
  paste0(as.POSIXlt(x)$year + 1900L,"m", as.POSIXlt(x)$mon+ 1L)
}

#' @export
#' @method format monthly
format.monthly <- function(x, ...){
  format(as.character(x),...)
}

#' @export
#' @method print monthly
print.monthly <- function(x, ...){
  print(format(x),...)
}

#' @export
#' @method as.data.frame monthly
as.data.frame.monthly <- function(...){
  as.data.frame.vector(...)
}

#' @export
#' @method mean monthly
mean.monthly <- function(x, ...) {as.monthly(NextMethod())}

#' @export
#' @method cut monthly
cut.monthly <- function(x, ...) {as.monthly(NextMethod())}

#' @export
#' @method seq monthly
seq.monthly <- function(x, ...) {as.monthly(NextMethod())}

#' @export
#' @method c monthly
c.monthly <- function(x, ...) {as.monthly(NextMethod())}

#' @export
#' @method rep monthly
rep.monthly <- function(x, ...) {as.monthly(NextMethod())}

#' @export
#' @method split monthly
split.monthly <- function(x, ...) {as.monthly(NextMethod())}

#' @export
#' @method as.list monthly
as.list.monthly <- function(x, ...) {as.monthly(NextMethod())}

#' @export
#' @method unique monthly
unique.monthly <- function(x, ...) {as.monthly(NextMethod())}

#' @export
#' @method [ monthly
`[.monthly` <- function(x, ...) {as.monthly(NextMethod())}










