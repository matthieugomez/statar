#' lead and lag with respect to a time variable
#'
#' @param x a vector of values
#' @param n a positive integer of length 1, giving the number of positions to lead or lag by. When the package lubridate is loaded, it can be a period when using with along_with (see the lubridate function minutes, hours, days, weeks, months and years)
#' @param along_with  time variable
#' @param default value used for non-existant rows. Defaults to \code{NA}.
#' @examples
#' year <- c(1989, 1991, 1992)
#' value <- c(4.1, 4.5, 3.3)
#' tlag(value, 1, along_with = year) #  returns value in year - 1
#' tlead(value, 1, along_with = year)
#' library(lubridate)
#' date <- mdy(c("01/04/1992", "03/15/1992", "04/03/1992"))
#' value <- c(4.1, 4.5, 3.3)
#' datem <- as.monthly(date)
#' tlag(value, along_with = datem) 
#' @name tlead-tlag
NULL


#' @export
#' @rdname tlead-tlag
tlead <- function(x, n = 1L, along_with, default = NA) {
  if (!is.numeric(n) | (length(n)>1)) stop("n must be a numeric of length one")
  index <- match(along_with + n, along_with, incomparables = NA) 
  out <- x[index]
  if (!is.na(default)) out[which(is.na(index))] <- default
  attributes(out) <- attributes(x)
  out
 }




#' @export
#' @rdname tlead-tlag
tlag <- function(x, n = 1L, along_with, default = NA) { 
  if (!is.numeric(n) | (length(n)>1)) stop("n must be a numeric of length one")
  index <- match(along_with - n, along_with, incomparables = NA)
  out <- x[index]
  if (!is.na(default)) out[which(is.na(index))] <- default
  attributes(out) <- attributes(x)
  out
}