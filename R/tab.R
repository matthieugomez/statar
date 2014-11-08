#' Returns cross tabulation
#' 
#' @param x a data.table
#' @param ... Variable to include. If length is two, a special cross tabulation table is printed although the a long data.table is always (invisibly) returned.
#' @param i Condition to apply function on certain rows only
#' @param w Weights. Default to NULL. 
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
#' @return a data.table sorted by variables in by, var, and with a new column "N" for counts.
#' @export
tab <- function(x, ..., i = NULL, w = NULL){
  tab_(x, vars = lazy_dots(...) , i = substitute(i), w = substitute(w))
}
#' @export
#' @rdname tab
tab_ <- function(x, vars = NULL, i = NULL, w = NULL){
  wvar <- names(select_vars_(names(x), w))
  if (!length(wvar)){
    wvar <- NULL
  }
  vars <- names(select_vars_(names(x), vars, exclude = c(wvar)))
  if (!is.null(i)){
    x <- eval(substitute(x[i, c(vars, wvar), with = FALSE]))
  } 
  if (length(vars)>1){
    byvar <- vars[length(vars)-1]
    var <- vars[length(vars)]
  } else{
    byvar <- character(0)
    var <- vars
  }
  if (is.null(wvar)){
    x <- x[, compute_count_vec(get(var)), by = c(byvar)]
  } else{
    x <- x[, compute_count_vec(get(var), w = get(wvar)), by = c(byvar)]
  }
  setDT(x)
  setnames(x, c(byvar, var, "N"))
  setkeyv(x, c(byvar, var, "N"))
  print_pretty_tab(x)
  invisible(x)
}

print_pretty_tab <- function(x){
  x <- copy(x)
  v <- vars_not_(x, "N")
  names <- names(x)
  if (length(names)==3){
    x1 <- length(unique(x[[1]]))
    x2 <- length(unique(x[[2]]))
    if (x1>=x2){
      id <-  names[1]
      variable <- names[2]
    } else{
      id <-  names[2]
      variable <- names[1]
    }
    x <- spread_(x, variable, "N", fill = 0)
    setkeyv(x, id)
    setnames(x, id, paste0(id, "\\\\", variable))
  } 
  stargazer(x, type = "text", summary = FALSE, rownames = FALSE)
}
