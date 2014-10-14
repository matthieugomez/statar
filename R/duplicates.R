#' returns duplicated rows
#'
#' @param x a data.table
#' @param ... Variables to keep (beyond the by variable). Default to all variables. See the \link[dplyr]{select} documentation.
#' @param by Variable to group by. Default is the key, or everything is the data.table is not keyed.
#' @param gen A character that specifies  the name of a new variable with the number of duplicates. Default to "N".
#' @param .dots Used to work around non-standard evaluation.
#' @return a data.table with groups that have duplicates. 
#' @examples
#' library(data.table)
#' DT <- data.table(a = rep(1:2, each = 3), b = 1:6)
#' duplicates(DT, by = "a")
#' duplicates(DT, by = list(a,b))
#' @export
duplicates <- function(x, ..., by = NULL, gen = "N"){
  duplicates_(x, .dots = lazyeval::lazy_dots(...), by = substitute(by), gen = gen)
}

#' @export
#' @rdname duplicates
duplicates_ <- function(x, ..., .dots, by = NULL, gen = "N"){
  stopifnot(is.data.table(x))
  if (gen %in% names(x))   stop(paste("A variable named", gen, "already exists."))
  if (anyDuplicated(names(x)))  stop("x has duplicate column names")
  dots <- lazyeval::all_dots(.dots, ...)
  byvars <- names(select_vars_(names(x), by))
  if (length(byvars)==0){
    byvars <- copy(names(x))
  }
  vars <- names(select_vars_(names(x), dots, exclude = byvars))
  x[, (gen) := .N-1,  by = c(byvars)]
  on.exit(x[, (gen) :=NULL])
  ans <- eval(substitute(x[NN>0, c(gen, byvars, vars), with = FALSE], list(NN = as.name(gen))))
  length <- nrow(ans)
  if (length >0){
    setkeyv(ans, c(gen, byvars))
    n_groups <- length- sum(duplicated(ans))
    message(paste(n_groups,"groups have duplicates"))
    setcolorder(ans, c(gen, byvars, setdiff(names(ans), c(byvars, gen))))
    return(ans)
  }
  else{
    message("There are no duplicates")
  }
}

