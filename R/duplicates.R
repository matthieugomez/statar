#' returns duplicated rows
#'
#' @param x a data.table
#' @param ... Variables to keep (beyond the by variable). Default to all variables. See the \link[dplyr]{select} documentation.
#' @param by Variable to group by. Default is the key, or everything is the data.table is not keyed.
#' @param gen A character that specifies  the name of a new variable with the number of duplicates. Default to "N".
#' @param vars Used to work around non-standard evaluation.
#' @return a data.table with groups that have duplicates. 
#' @examples
#' library(data.table)
#' DT <- data.table(a = rep(1:2, each = 3), b = 1:6)
#' duplicates(DT, by = "a")
#' duplicates(DT, by = list(a,b))
#' @export
duplicates <- function(x, ..., by = NULL, gen = "N"){
  duplicates_(x, vars = lazyeval::lazy_dots(...), by = substitute(by), gen = gen)
}

#' @export
#' @rdname duplicates
duplicates_ <- function(x, vars, by = NULL, gen = "N"){
  stopifnot(is.data.table(x))
  names <- names(x)
  if (gen %in% names)   stop(paste("A variable named", gen, "already exists."))
  if (anyDuplicated(names))  stop("x has duplicate column names")
  dots <- lazyeval::all_dots(vars)
  byvars <- names(select_vars_(names, by))
  if (!length(byvars)){
    if (length(key(x))){
      byvars <- key(x)
    } else{
      byvars <- names(x)
    }
  }
  vars <- names(select_vars_(names, dots, exclude = byvars))
  if (length(vars)==0){
    vars <- setdiff(names(x), byvars)
  }

  ans <- x[, .I[.N>1L], by=c(byvars)]
  ans <- ans[[length(ans)]]
  ans <- x[ans, c(byvars, vars), with = FALSE]
  n_groups <- nrow(unique(ans, by = byvars))
  message(paste(n_groups,"groups have duplicates"))

  if (nrow(ans)){
    ans[, c(gen) := .N, by = c(byvars)]
    setkeyv(ans, c(gen, byvars))
    setcolorder(ans, c(gen, byvars, setdiff(names(ans), c(byvars, gen))))
  }
  return(ans[])

}

