#' returns number of unique element
#'
#' @param x a vector or a data.table
#' @param by 
#' @return a data.table with groups that have duplicates. 
#' @examples
#' n_unique(c(1,2,3))
#' library(data.table)
#' DT <- data.table(a = rep(1:2, each = 3), b = 1:6)
#' n_unique(DT)
#' n_unique(DT, by = "a")
#' @export
n_unique <- function(x, ...){
    UseMethod("n_unique")
}

#' @export
n_unique.data.table <- function(x, ..., by = key(x)){
    nrow(x) - sum(duplicated(x, by = by))
}

#' @export
n_unique.default <- function(x, ...){
    n_distinct(x)
}

