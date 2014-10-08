#' Create unique names within environment
#'
#' @param prefix A character that specifies prefix for new name
#' @param where A list or an environment
#' @param inherits  Should the name unique also in the enclosing frames of the environment?
#' @examples
#' temp <- 1
#' tempname <- tempname("temp", globalenv())
#' DT <- data.table(
#'  temp = c(1, 1, 1, 1, 1, 2, 2), 
#'  temp1 = c(1992, 1989, 1991, 1990, 1994, 1992, 1991), 
#')
#' tempvar <- tempname("temp", DT)
#' tempvar <- tempname("temporary", DT)
#' @export
tempname=function(prefix, where, inherits=TRUE) {
    i <- 0L
    name <- prefix
    while (exists(name, where = where, inherits = inherits)) {
        i <- i + 1L
        name <- paste0(prefix, as.character(i))
    }
    name
}






