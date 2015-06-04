#' Returns cross tabulation
#' 
#' @param x a vector or a data.frame
#' @param ... Variable to include. If length is two, a special cross tabulation table is printed although the a long data.frame is always (invisibly) returned.
#' @param i Condition to apply function on certain rows only
#' @param w Frequency weights. Default to NULL. 
#' @param na.rm Remove missing values. Default to FALSE
#' @param vars Used to work around non-standard evaluation.
#' @param sort Boolean. Default to TRUE
#' @examples
#' library(dplyr)
#' N <- 1e2 ; K = 10
#' df <- data_frame(
#'   id = sample(c(NA,1:5), N/K, TRUE),
#'   v1 =  sample(1:5, N/K, TRUE)                       
#' )
#' tab(df[["id"]])
#' tab(df, id)
#' df %>% group_by(id) %>% tab()
#' df %>% group_by(id) %>% tab(v1)
#' tab(df, id, i = id>=3)
#' @return a data.frame sorted by variables in ..., and with a columns "n", "Percent", and "Cum." for counts.
#' @export
tab <- function(x, ...) {
  UseMethod("tab")
}

#' @export
#' @method tab default
tab.default <- function(x, ..., w = NULL, na.rm = FALSE, sort = TRUE) {
  x <- setNames(data.frame(x), "x")
  x <- group_by_(x, .dots =  "x")
  x <- count_(x, vars = "x", wt = w)
  x <- mutate_(x, .dots = setNames(list(~n/sum(n)*100), "Percent"))
  x <- mutate_(x, .dots = setNames(list(~cumsum(Percent)), "Cum"))
  if (na.rm){
     x <- na.omit(x)
   }
   if (sort){
     x <- arrange_(x, .dots = "x")
   }
   print_pretty_tab(x)
   invisible(x)
}

#' @export
#' @method tab data.frame
tab.data.frame <- function(x, ..., i = NULL, w = NULL, na.rm = FALSE, sort = TRUE){
  tab_(x, vars = lazy_dots(...) , i = lazy(i), w = substitute(w), na.rm = na.rm, sort = sort)
}


#' @export
#' @rdname tab
tab_ <- function(x, vars = NULL, i = NULL, w = NULL, na.rm = FALSE, sort = sort){
  byvars <-  vapply(groups(x), as.character, character(1))
  wvar <- names(select_vars_(names(x), w))
  if (!length(wvar)){
    wvar <- NULL
  }
  vars <- names(select_vars_(names(x), vars, exclude = c(wvar, byvars)))
  vars <- c(byvars, vars)
  if (!is.null(i$expr)){
    newname <- tempname(x, 1)
    x <- mutate_(x, .dots = setNames(list(i), newname))
    x <- select_(x, .dots = c(vars, wvar, newname))
    x <- filter_(x, .dots = interp(~var, var = as.name(newname)))
  } 
  x <- count_(x, vars = vars, wt = w)
  x <- mutate_(x, .dots = setNames(list(~n/sum(n)*100), "Percent"))
  x <- mutate_(x, .dots = setNames(list(~cumsum(Percent)), "Cum"))
  if (na.rm){
    x <- na.omit(x)
  }
  if (sort){
    x <- arrange_(x, .dots = vars)
  }
  print_pretty_tab(x)
  invisible(x)
}


print_pretty_tab <- function(x){
  stargazer(format(x, digits = 3), type = "text", summary = FALSE, rownames = FALSE)
}


