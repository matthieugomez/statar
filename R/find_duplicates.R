#' returns a data.frame with duplicated rows
#'
#' @param x a data.frame
#' @param ... Variable on which one should check for duplicates. Default to all variables
#' @param gen A character that specifies  the name of a new variable with the number of duplicates. Default to "N".
#' @param vars Used to work around non-standard evaluation.
#' @return a data.frame with groups that have duplicates. 
#' @examples
#' df <- data.frame(a = rep(1:2, each = 3), b = 1:6)
#' find_duplicates(df, a)
#' @export
find_duplicates <- function(x, ..., gen = "N"){
  find_duplicates_(x, vars = lazyeval::lazy_dots(...), gen = gen)
}

#' @export
#' @rdname find_duplicates
find_duplicates_ <- function(x, vars, gen = "N"){
  names <- names(x)
  if (gen %in% names)   stop(paste("A variable named", gen, "already exists."))
  if (anyDuplicated(names))  stop("x has duplicate column names")
  dots <- lazyeval::all_dots(vars)
  byvars <- names(select_vars_(names, dots))
  byvars <-  c(vapply(groups(x), as.character, character(1)), byvars)
  ans <- mutate_(group_by_(x, .dots = byvars), .dots = setNames(list(~n()), gen))
  ans <- filter_(ans, interp(~gen > 1, gen = as.name(gen)))
  n_groups <- nrow(ans)
  message(paste(n_groups, "groups have duplicates"))
  if (n_groups>0){
    ans <- arrange_(ans, .dots = c(gen, byvars))
    ans <- select_(ans, interp(~gen, gen =as.name(gen)), interp(~byvars, byvars =as.name(byvars)), ~everything())
  } 
  return(ans)
}

