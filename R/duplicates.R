#' returns rows in duplicated groups
#'
#' @param x a data.table
#' @param ... Variables to keep (beyond the by variable). Default to all variables. See the \link[dplyr]{select} documentation.
#' @param by Variable to group by. Default is the key, or everything is the data.table is not keyed.
#' @return a data.table with rows in groups that have duplicates. The first column is a new variable, called "N" (N-i if N already exists), that countains the number of duplicates
#' @examples
#' DT <- data.table(a = rep(1:2, each = 3), b = 1:6)
#' duplicates(DT, by = "a")
#' duplicates(DT, by = list(a,b))
#' @export
duplicates <- function(x, ..., by = NULL){
  duplicates_(x, .dots = lazyeval::lazy_dots(...), by = substitute(by))
}

#' @export
#' @rdname duplicates
duplicates_ <- function(x, ..., .dots, by = NULL){
  stopifnot(is.data.table(x))
  N <- tempname("N", x)
  if (anyDuplicated(names(x))){
    stop("x has duplicate column names")
  }
  dots <- lazyeval::all_dots(.dots, ...)
  byvars <- names(select_vars_(names(x), by))
  if (length(byvars)==0){
    byvars <- copy(names(x))
  }
  vars <- names(select_vars_(names(x), dots, exclude = byvars))
  x[, (N) := .N-1,  by = c(byvars)]
  on.exit(x[, (N) :=NULL])
  ans <- eval(substitute(x[NN>0, c(N, byvars, vars), with = FALSE], list(NN = as.name(N))))
  length <- nrow(ans)
  if (length >0){
    setkeyv(ans, c(N,byvars))
    n_groups <- length- sum(duplicated(ans))
    message(paste(n_groups,"groups have duplicates"))
    setcolorder(ans, c(N, byvars, setdiff(names(ans), c(byvars, N))))
    return(ans)
  }
  else{
    message("There are no duplicates")
  }
}

