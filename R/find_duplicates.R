#' returns a data.frame with duplicated rows
#'
#' @param x a data.frame
#' @param ... Variable on which one should check for duplicates. Default to all variables
#' @param vars Used to work around non-standard evaluation.
#' @return a data.frame with groups that have duplicates. 
#' @examples
#' df <- data.frame(a = rep(1:2, each = 3), b = 1:6)
#' find_duplicates(df, a)
#' @export
find_duplicates <- function(x, ...){
  find_duplicates_(x, vars = lazyeval::lazy_dots(...))
}

#' @export
#' @rdname find_duplicates
find_duplicates_ <- function(x, vars){
  names <- names(x)
  if ("n" %in% names)   stop(paste("A variable named n already exists."))
  if (anyDuplicated(names))  stop("x has duplicate column names")
  dots <- lazyeval::all_dots(vars)
  byvars <- names(select_vars_(names, dots))
  byvars <-  c(vapply(groups(x), as.character, character(1)), byvars)
  ans <- group_by_(x, .dots = byvars)
  ans <- mutate_(ans, .dots = setNames(list(~n()), "n"))
  ans <- filter_(ans, ~ n > 1)
  n_groups <- nrow(distinct_(ans, .dots = byvars))
  message(paste(n_groups, "groups have duplicates"))
  if (n_groups>0){
    ans <- arrange_(ans, .dots = c("n", byvars))
    ans <- select_(ans, ~n, interp(~byvars, byvars =as.name(byvars)), ~everything())
  } 
  return(ans)
}

