#' Find string that identifies an id
#'
#' \code{fuzzy_join} uses record linkage methods to match observations between two datasets where no perfect key fields exist.  For each row in x, \code{fuzzy_join} finds the closest row(s) in y. The distance is a weighted average of the string distances defined in \code{method} over multiple columns.
#'
#' @param id identifier
#' @param v string associated to id


id <- c(1, 1, 2, 2)
v <- c("apple orange", "apple", "orange strawberries", "new")


count <- function(id, v){
  dt <- setDT(list(id = id, v = v))
  f <- function(x){unlist(str_split(x, "\\s+"))}
  dt <- dt[, list(v = f(v)), by = id]
  dt <- dt[, list(.N) , by = c("id", "v")]
  setnames(dt, c("id", "v", "count_within"))
  dt[, count_across := .N, by = v]
  dt[]
}






