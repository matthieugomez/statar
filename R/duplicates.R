#' returns groups with duplicates
#'
#' @param x a data.table
#' @param by a character vectors. Default is key of data.table
#' @return a data.table composed of groups that have duplicates. The first column is a new variable, named "N", that displays the number of duplicates
#' @examples
#' DT <- data.table(a = rep(1:2, each = 3), b=1:6)
#' duplicates(DT, by = "a")
#' @export
duplicates <- function(x, by = key(x)){
  if ("N" %in% names(x))
    stop("Variable N already exists")
  if (anyDuplicated(names(x))){
    stop("x has duplicate column names")
  }
  if (length(by)==0){
    by <- copy(names(x))
  }
  x[, N := .N-1,  by = by]
  on.exit(x[, N :=NULL])
  ans <- x[N>0]
  setkeyv(ans, c("N",by))
  setcolorder(ans, c("N", by, setdiff(names(ans),c(by,"N"))))
  message(paste(sum(duplicated(ans))," groups have duplicates"))
  ans
}

