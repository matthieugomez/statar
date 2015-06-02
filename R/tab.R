#' Returns cross tabulation
#' 
#' @param x a vector or a data.table
#' @param ... Variable to include. If length is two, a special cross tabulation table is printed although the a long data.table is always (invisibly) returned.
#' @param i Condition to apply function on certain rows only
#' @param w Frequency weights. Default to NULL. 
#' @param na.omit Omit missing values. Default to FALSE
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
#' tab(DT, id, i = v1>=3)
#' tab(DT, id, v1, w = v2)
#' @return a data.table sorted by variables in ..., and with a columns "Freq", "Percent", and "Cum." for counts.
#' @export
tab <- function(x, ...) {
  UseMethod("tab")
}

#' @export
#' @method tab default
tab.default <- function(x, ..., w = NULL, na.omit = FALSE) {
  xsub <- copy(deparse(substitute(x)))
  x <- data_frame(x)
  x <- setNames(x, xsub)
  xsub <- paste0("`", xsub, "`")
  x <- group_by_(x, .dots =  xsub)
  if (is.null(w)){
     x <- summarize(x, Freq = n()) 
  } else{
       new = list(~sum(w, na.rm = TRUE))
       x <- summarize_(x, .dots = setNames(new, "Freq")) 
  }
  x <- mutate(x, Percent = Freq/sum(Freq)*100, Cum = cumsum(Percent))
  if (na.omit){
    x <- na.omit(x)
  }
  x
}

#' @export
#' @method tab data.table
tab.data.table <- function(x, ..., i = NULL, w = NULL, na.omit = FALSE){
  tab_(x, vars = lazy_dots(...) , i = lazy(i), w = lazy(w), na.omit = na.omit)
}
#' @export
#' @rdname tab
tab_ <- function(x, vars = NULL, i = NULL, w = NULL, na.omit = FALSE){
  wvar <- names(select_vars_(names(x), w$expr))
  if (!length(wvar)){
    wvar <- NULL
  }
  vars <- names(select_vars_(names(x), vars, exclude = c(wvar)))
  if (!is.null(i$expr)){
    x <- filter_(x, .dots = i)
  } 
  x <- select(x, one_of(c(vars, wvar)))
  x <- group_by_(x, .dots = vars)
  if (is.null(wvar)){
    x <- summarize(x, Freq = n()) 
  } else{
    new = list(interp(~sum(w, na.rm = TRUE), w = w))
    x <- summarize_(x, .dots = setNames(new, "Freq")) 
  }
  x <- mutate(x, Percent = Freq/sum(Freq)*100, Cum = cumsum(Percent))
  if (na.omit){
    x <- na.omit(x)
  }
  x <- arrange_(x, .dots = vars)
  print_pretty_tab(x)
  invisible(x)
}


print_pretty_tab <- function(x){
  print(x, row.names = FALSE)
}


