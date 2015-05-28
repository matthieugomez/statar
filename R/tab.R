#' Returns cross tabulation
#' 
#' @param x a vector or a data.table
#' @param ... Variable to include. If length is two, a special cross tabulation table is printed although the a long data.table is always (invisibly) returned.
#' @param i Condition to apply function on certain rows only
#' @param w Frequency weights. Default to NULL. 
#' @param vars Used to work around non-standard evaluation.
#' @examples
#' library(data.table)
#' N <- 1e2 ; K = 10
#' DT <- data.table(
#'   id = sample(c(NA,1:5), N/K, TRUE),
#'   v1 =  sample(c(NA,1:5), N, TRUE),
#'   v2 =  sample(c(NA,1:1e6), N, TRUE)                       
#' )
#' tab(DT[["id"]])
#' tab(DT, id)
#' tab(DT, id, v1, w = v2,)
#' @return a data.table sorted by variables in ..., and with a columns "Freq", "Percent", and "Cum." for counts.
#' @export
tab <- function(x, ...) {
  UseMethod("tab")
}

#' @export
#' @method tab default
tab.default <- function(x, w = NULL, na.omit = TRUE) {
  xsub <- copy(deparse(substitute(x)))
  x <- list(x)
  setDT(x)
  setnames(x, xsub)
  if (is.null(w)){
    x[,list(Obs = .N), keyby = c(xsub)]
  } else{
    x[,list(Obs = sum(w, na.rm = TRUE)), keyby = c(xsub)]
  }
  if (na.omit){
    x <- na.omit(x)
  }
}

#' @export
#' @method tab data.table
tab.data.table <- function(x, ..., i = NULL, w = NULL, na.omit = TRUE){
  tab_(x, vars = lazy_dots(...) , i = substitute(i), w = substitute(w), na.omit = na.omit)
}
#' @export
#' @rdname tab
tab_ <- function(x, vars = NULL, i = NULL, w = NULL, na.omit = TRUE){
  wvar <- names(select_vars_(names(x), w))
  if (!length(wvar)){
    wvar <- NULL
  }
  vars <- names(select_vars_(names(x), vars, exclude = c(wvar)))
  if (!is.null(i)){
    x <- eval(substitute(x[i, c(vars, wvar), with = FALSE]))
  } 
  if (is.null(wvar)){
    x <- x[, list(Freq = .N) , keyby = c(vars)]
  } else{
    x <- x[, list(Freq = sum(get(wvar))), keyby = c(vars)]
  }
  x[, Percent := Freq/sum(Freq)*100]
  x[, Cum. := cumsum(Percent)]

  if (na.omit){
    x <- na.omit(x)
  }
  print_pretty_tab(x)
  invisible(x)
}


print_pretty_tab <- function(x){
  print(x, row.names = FALSE)
}


