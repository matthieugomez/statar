#' Elapsed calendar periods
#'
#' @param date Any vector that has a method for "
#' @examples 
#' date <- mdy(c("04/03/1992", "01/04/1992", "03/15/1992"))  
#' l <- data.frame(
#'    weekly = as.weekly(date), 
#'    monthly = as.monthly(date), 
#'    quarterly = as.quarterly(date)
#'    )
#' l
#' l+1
#' l %>% mutate_each(funs(week))
#' l %>% mutate_each(funs(lag(c(1,2,3), along_with = .)))
#' @details Weekly, monthly and quarterly dates are dates stored as the number of elapsed calendar periods since January 01 1970. This is helpful for two reasons. First, it allows to use methods defined on numeric on quarterly date (for instance lag wrt one period). Moreover, these classes are printed in a way that fits their frequency YYYYqQ, YYYYmMM, YYYYmWW.  Methods to convert from and to Dates or POSIXlt are provided.
#' @export
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
    date <- floor_date(as.POSIXlt(date), "week")
    date_origin <- as.POSIXlt(mdy("01/01/1970"))
    out <- 53L*(date$year-date_origin$year) + ((date$yday)%/%7 -date_origin$mon)
    attributes(out) <- NULL
    class(out) <-  "weekly"
    out
}

# as.POSIXlt
#' @export
#' @rdname elapsed 
as.POSIXlt.quarterly <- function(date, ...){
  attributes(date) <- NULL
  date_origin <- mdy("01/01/1970")
  date <- date_origin+ 3L*months(date)
  as.POSIXlt(date, ...)
}

#' @export
#' @rdname elapsed 
as.POSIXlt.monthly <- function(date, ...){
  attributes(date) <- NULL
  date_origin <- mdy("01/01/1970")
  date <- date_origin+ months(date)
  as.POSIXlt(date, ...)
}

#' @export
#' @rdname elapsed 
as.POSIXlt.weekly<- function(date, ...){
  attributes(date) <- NULL
  date_origin <- mdy("01/01/1970")
  date <- date_origin+ weeks(date)
  as.POSIXlt(date, ...)
}

# as.Date
#' @export
#' @rdname elapsed 
as.Date.quarterly <- function(date, ...){
  attributes(date) <- NULL
  date_origin <- mdy("01/01/1970")
  date <- date_origin+ 3L*months(date)
  as.Date(date, ...)
}

#' @export
#' @rdname elapsed 
as.Date.monthly <- function(date, ...){
  attributes(date) <- NULL
  date_origin <- mdy("01/01/1970")
  date <- date_origin+ months(date)
  as.Date(date, ...)
}

#' @export
#' @rdname elapsed 
as.Date.weekly<- function(date, ...){
  attributes(date) <- NULL
  date_origin <- mdy("01/01/1970")
  date <- date_origin+ weeks(date)
  as.Date(date, ...)
}



# format (align on m and q)
#' @export
#' @rdname elapsed 
format.quarterly <- function(date, ...){
  format(paste0(year(date),"q", quarter(date)), ...)
}

#' @export
#' @rdname elapsed 
format.monthly <- function(date, ...){
  month <- month(date)
  monthc <- as.character(month)
  monthcc <- paste0("0",monthc[which(month<10)])
  monthc[which(month<10)] <- monthcc
  format(paste0(year(date),"m", monthc), ...)
}

#' @export
#' @rdname elapsed 
format.weekly <- function(date, ...){
  week <- week(date)
  weekc <- as.character(week)
  weekcc <- paste0("0", weekc[which(week<10)])
  weekc[which(week<10)] <- weekcc
  format(paste0(year(date),"w", weekc), ...)
}

# stupid stuff
#' @export
#' @rdname elapsed 
as.data.frame.monthly <- function(...){
      as.data.frame.vector(...)
}

#' @export
#' @rdname elapsed 
as.data.frame.quarterly <- function(...){
    as.data.frame.vector(...)
}

#' @export
#' @rdname elapsed 
as.data.frame.weekly <- function(...){
    as.data.frame.vector(...)
}

#' @export
#' @rdname elapsed 
print.quarterly <- function(date){
  print(format(date))
}

#' @export
#' @rdname elapsed 
print.monthly <- function(date){
  print(format(date))
}

#' @export
#' @rdname elapsed 
print.weekly <- function(date){
  print(format(date))
}
