#' Returns cross tabulation
#' 
#' @param x a data.table
#' @param ... Variables to include. Defaults to all non-grouping variables. See the \link[dplyr]{select} documentation.
#' @param w Weights. Default to NULL. 
#' @param i Condition to apply function on certain rows only
#' @param vars Used to work around non-standard evaluation.
#' @examples
#' library(data.table)
#' N <- 1e2 ; K = 10
#' DT <- data.table(
#'   id = sample(c(NA,1:5), N/K, TRUE),
#'   v1 =  sample(c(NA,1:5), N, TRUE),
#'   v2 =  sample(c(NA,1:1e6), N, TRUE)                       
#' )
#' tab(DT, id)
#' tab(DT, id, v1, w = v2)
#' @return a data.table with cross tabulation. NA are values like others (always displayed if present)
#' @export
tab <- function(x, ..., i = NULL, w = NULL){
  tab_(x, vars = lazy_dots(...) , i = substitute(i), w = substitute(w))
}

#' @export
#' @rdname tab
tab_ <- function(x, vars, i = NULL, w = NULL){
  wvar <- names(select_vars_(names(x), w))
  if (!length(wvar)){
    wvar <- NULL
  }
  vars <- names(select_vars_(names(x), vars, exclude = wvar))
  if (!is.null(i)){
    x <- eval(substitute(x[i, c(vars, wvar), with = FALSE]))
  } 
  if (length(vars)==1){
    if (is.null(wvar)){
      x <- compute_count_vec(x[[vars]])
    }
      else{
        x <- compute_count_vec(x[[vars]], w = x[[wvar]])
    }
    setnames(x, c(vars, "count"))
  } else{
    if (is.null(wvar)){
      x <- x[, compute_count_vec(get(vars[2])), by = c(vars[1])]
    } else{
      x <- x[, compute_count_vec(get(vars[2]), w = get(wvar)), by = c(vars[1])]
    }
    x <- spread_(x, "x_", "count_", fill = 0) 
    setDT(x)
    setnames(x, vars[1], paste0(vars, collapse = "\\"))
  }
  x
}



