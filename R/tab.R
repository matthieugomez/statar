#' Returns cross tabulation
#' 
#' @param x A vector or a data.frame.
#' @param ... Variable(s) to include. If length is two or more, a special cross tabulation table is printed although a long data.frame is always (invisibly) returned.
#' @param i Condition to apply function on certain rows only.
#' @param w Frequency weights. Default to NULL.
#' @param na.rm Remove missing values. Default to FALSE.
#' @param .dots Used to work around non-standard evaluation.
#' @param sort Boolean. Default to TRUE.
#' @examples
#' # setup
#' library(dplyr)
#' N <- 1e2 ; K = 10
#' df <- data_frame(
#'   id = sample(c(NA,1:5), N/K, TRUE),
#'   v1 = sample(1:5, N/K, TRUE)                       
#' )
#' 
#' # one-way tabulation
#' tab(df[["id"]])
#' tab(df, id)
#' tab(df, id, i = id>=3)
#' df %>% group_by(id) %>% tab
#' df %>% tab(id)
#' 
#' # two-way tabulation
#' df %>% group_by(id) %>% tab(v1)
#' df %>% tab(id, v1)
#' @return a data.frame sorted by variables in ..., and with columns "Freq.", "Percent", and "Cum." for counts.
#' @export
tab <- function(x, ...) {
  UseMethod("tab")
}

#' @export
#' @method tab default
tab.default <- function(x, ..., w = NULL, na.rm = FALSE, sort = TRUE) {
  x <- setNames(data.frame(x), "x")
  x <- group_by_(x, .dots =  "x")
  if (na.rm){
    x <- select_(x, .dots = "x")
    x <- na.omit(x)
  }
  x <- count_(x, vars = "x", wt = w)
  x <- mutate_(x, .dots = setNames(list(~n), "Freq."))
  x <- mutate_(x, .dots = setNames(list(~formatC(n/sum(n)*100, digits = 1L, format = "f")), "Percent"))
  x <- mutate_(x, .dots = setNames(list(~formatC(cumsum(Percent), digits = 1L, format = "f")), "Cum."))
  if (sort){
    x <- arrange_(x, .dots = "x")
  }
  x <- select(x, -n)
  if (ncol(x) == 4) {
    total_freq <- formatC(sum(x[, 2]), digits = 0L, format = "f")
    x <- sapply(x, as.character)
    x <- rbind(x, c("Total", total_freq, "100.0", "\u00a0"))
    x[nrow(x) - 1L, ncol(x)] <- "100.0"
    x <- as_data_frame(x)
    statascii(x, flavor = "oneway")
  }
  else if (ncol(x) > 4) {
    statascii(x, flavor = "summary", separators = TRUE)
  }
  invisible(x)
}

#' @export
#' @method tab data.frame
tab.data.frame <- function(x, ..., i = NULL, w = NULL, na.rm = FALSE, sort = TRUE){
  tab_(x, .dots = lazy_dots(...) , i = lazy(i), w = substitute(w), na.rm = na.rm, sort = sort)
}

#' @export
#' @rdname tab
tab_ <- function(x, ..., .dots, i = NULL, w = NULL, na.rm = FALSE, sort = sort){
  byvars <- as.character(groups(x))
  wvar <- select_vars_(names(x), w)
  if (!length(wvar)){
    wvar <- NULL
  }
  dots <- all_dots(.dots, ..., all_named = TRUE)
  vars <- select_vars_(names(x), dots, exclude = c(wvar, byvars))
  vars <- c(byvars, vars)
  if (!is.null(i$expr)){
    newname <- tempname(x, 1)
    x <- mutate_(x, .dots = setNames(list(i), newname))
    x <- select_(x, .dots = c(vars, wvar, newname))
    x <- filter_(x, .dots = interp(~var, var = as.name(newname)))
  }
  if (na.rm){
    x <- select_(x, .dots = vars)
    x <- na.omit(x)
  }
  x <- count_(x, vars = vars, wt = w)
  x <- mutate_(x, .dots = setNames(list(~n), "Freq."))
  x <- mutate_(x, .dots = setNames(list(~formatC(n/sum(n)*100, digits = 1L, format = "f")), "Percent"))
  x <- mutate_(x, .dots = setNames(list(~formatC(cumsum(Percent), digits = 1L, format = "f")), "Cum."))
  if (sort){
    x <- arrange_(x, .dots = vars)
  }
  x <- select(x, -n)
  if (ncol(x) == 4) {
    total_freq <- formatC(sum(x[, 2]), digits = 0L, format = "f")
    x <- sapply(x, as.character)
    x <- rbind(x, c("Total", total_freq, "100.0", "\u00a0"))
    x[nrow(x) - 1L, ncol(x)] <- "100.0"
    x <- as_data_frame(x)
    statascii(x, flavor = "oneway")
  }
  else if (ncol(x) > 4) {
    statascii(x, flavor = "summary", separators = TRUE)
  }
  invisible(x)
}