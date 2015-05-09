#' returns a data.table with duplicated rows
#'
#' @param x a data.table
#' @param ... Variable on which one should check for duplicates. Default to all variables
#' @param gen A character that specifies  the name of a new variable with the number of duplicates. Default to "N".
#' @param vars Used to work around non-standard evaluation.
#' @return a data.table with groups that have duplicates. 
#' @examples
#' library(data.table)
#' DT <- data.table(a = rep(1:2, each = 3), b = 1:6)
#' find_duplicates(DT, a)
#' find_duplicates(DT, a, b)
#' @export
find_duplicates <- function(x, ..., gen = "N"){
  find_duplicates_(x, vars = lazyeval::lazy_dots(...), gen = gen)
}

#' @export
#' @rdname find_duplicates
find_duplicates_ <- function(x, vars, gen = "N"){
  stopifnot(is.data.table(x))
  names <- names(x)
  if (gen %in% names)   stop(paste("A variable named", gen, "already exists."))
  if (anyDuplicated(names))  stop("x has duplicate column names")
  dots <- lazyeval::all_dots(vars)
  byvars <- names(select_vars_(names, dots))
  if (!length(byvars)){
      byvars <- names(x)
  }
  ans <- x[, .I[.N>1L], by=c(byvars)]
  ans <- ans[[length(ans)]]
  ans <- x[ans]
  n_groups <- nrow(unique(ans, by = byvars))
  message(paste(n_groups, "groups have duplicates"))
  if (n_groups>0){
    ans <- ans[, c(gen) := .N, by = c(byvars)]
    setkeyv(ans, c(gen, byvars))
    setcolorder(ans, c(gen, byvars, setdiff(names(ans), c(byvars, gen))))
  } 
  return(ans[])
}

