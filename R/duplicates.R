#' Returns a data.table with duplicated groups
#'
#' @param x a data.table
#' @param ... Variables to keep (beyond the grouped). Default to all variables. See the \link[dplyr]{select} documentation.
#' @param by Variable to group by. Default is the key, or everything is the data.table is not keyed.
#' @return a data.table composed of groups that have duplicates. The first column is a new variable, named "N", that displays the number of duplicates
#' @examples
#' DT <- data.table(a = rep(1:2, each = 3), b = 1:6)
#' duplicates(DT, by = "a")
#' @export
duplicates <- function(x, ..., by = NULL){
  duplicates_(x, .dots = lazyeval::lazy_dots(...), by = substitute(by))
}

#' @export
#' @rdname duplicates
duplicates_ <- function(x, ..., .dots, by = NULL){
  if ("N" %in% names(x))
    stop("Variable N already exists")
  if (anyDuplicated(names(x))){
    stop("x has duplicate column names")
  }
  dots <- lazyeval::all_dots(.dots, ...)
  byvars <- names(select_vars_(names(x), by))
  if (length(byvars)==0){
    byvars <- copy(names(x))
  }
  vars <- names(select_vars_(names(x), dots, exclude = byvars))
  x[, N := .N-1,  by = c(byvars)]
  on.exit(x[, N :=NULL])
  ans <- x[N>0, c("N",byvars,vars), with = FALSE]
  n_groups <- sum(duplicated(ans))
  message(paste(n_groups," groups have duplicates"))
  if (n_groups >0){
    setkeyv(ans, c("N",byvars))
    setcolorder(ans, c("N", byvars, setdiff(names(ans),c(byvars,"N"))))
    ans
  }
}

