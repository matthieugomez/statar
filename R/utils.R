dt_env <- function(dt, env, byvars) {
  env <- new.env(parent = env, size = 2L)
  env$dt <- dt
  env$byvars <- byvars
  env
}


and_expr = function (exprs) 
{
    if (length(exprs) == 0) 
        return(TRUE)
    if (length(exprs) == 1) 
        return(exprs[[1]])
    left <- exprs[[1]]
    for (i in 2:length(exprs)) {
        left <- substitute(left & right, list(left = left, right = exprs[[i]]))
    }
    left
}


or_expr = function (exprs) 
{
    if (length(exprs) == 0) 
        return(TRUE)
    if (length(exprs) == 1) 
        return(exprs[[1]])
    left <- exprs[[1]]
    for (i in 2:length(exprs)) {
        left <- substitute(left | right, list(left = left, right = exprs[[i]]))
    }
    left
}


deparse_all <- function(x) {
  deparse2 <- function(x) paste(deparse(x, width.cutoff = 500L), collapse = "")
  vapply(x, deparse2, FUN.VALUE = character(1))
}


shallow <- function(x,...){
  shallow_(x = x, vars = lazyeval::lazy_dots(...))
}

shallow_ <- function(x, vars) {
    vars <- names(select_vars_(names(x), vars))
    if (length(vars) == 0) {
       vars <- names(x)
    }
    out = as.list(x)[vars]
    setDT(out)
    out
}


wquantile <- function(x, probs = c(0.25, 0.5, 0.75), w = NULL, na.rm = FALSE){
  if (is.null(w)){
    quantile(x = x, type = 2, probs = probs, na.rm = na.rm)
  } else{
      # implementation of quantile type = 2 with weight
      if (anyNA(x) | anyNA(w)) {
        if (na.rm) {
          na <- is.na(x) | is.na(w)
          x <- x[!na]
          w <- w[!na]
        }
        else{
          stop("Missing values not allowed when na.rm is FALSE", call. = FALSE)
        } 
      }
      # Ensure x and w in ascending order of x
      order <- order(x)
      cumsum <- cumsum(w[order])
      n <- cumsum[length(cumsum)]

      # follow definition of quantile 2 in R
      index <- n * probs
      j <- floor(index)
      low <- x[order[pmin(length(x),   .bincode(j, c(-Inf, cumsum)))]]
      high <- x[order[pmin(length(x),   .bincode(j + 1, c(-Inf, cumsum)))]]
      ifelse(j == index, 0.5 * low + 0.5 * high, high)
    }
}






assign_var <- function(x, ..., env = parent.frame(), inherits = FALSE){
    names <- sapply(lazy_dots(...), function(x){as.character(x$expr)})
    assign_var_(x = x, names = names, inherits = inherits, env = parent.frame())
}
assign_var_ <- function(x, names, env = parent.frame(), inherits=TRUE) {
    for (name in names){
        tempname <- tempname(paste("temp",name,sep="_"), where = env, inherits = inherits)
        assign(name, tempname, env)
    }
}


setmutate <- function(x, ..., i = NULL, by = NULL){
    setmutate_(x, vars = lazy_dots(...), i = substitute(i), by = substitute(by))
}

setmutate_ <- function(x, vars, i = NULL, by = NULL){
    stopifnot(is.data.table(x))
    byvars <- names(select_vars_(names(x), by))
    if (!length(by)){
        byvars <- NULL
    }
    if (!is.null(i)){
      i <- as.lazy(i)$expr
    }
    dots <- lazyeval::all_dots(vars, all_named = TRUE)
    env <- dt_env(x, lazyeval::common_env(dots), byvars)
     # For each new variable, generate a call of the form df[, new := expr]
     for(col in names(dots)) {
      if (is.null(byvars) & is.null(i)){
         call <- substitute(dt[, lhs := rhs],
           list(lhs = as.name(col), rhs = dots[[col]]$expr))
       } else if (is.null(byvars) & !is.null(i)){
        call <- substitute(dt[i, lhs := rhs],
          list(lhs = as.name(col), rhs = dots[[col]]$expr, i = i))
      } else if (!is.null(byvars) & is.null(i)){
        call <- substitute(dt[, lhs := rhs, by = c(byvars)],
          list(lhs = as.name(col), rhs = dots[[col]]$expr))
      } else {
        call <- substitute(dt[i, lhs := rhs, by = c(byvars)],
          list(lhs = as.name(col), rhs = dots[[col]]$expr, i = i))
      }
    eval(call, env)
    }
  x[]
}

setmutate_each <- function(x, funs, ..., i = NULL, by = NULL, replace = FALSE){
    setmutate_each_(x, funs, vars = lazy_dots(...), i = substitute(i), by = substitute(by), replace = replace)
}

setmutate_each_ <- function(x, funs, vars, i = NULL, by = NULL, replace = FALSE){
  stopifnot(is.data.table(x))
    if (anyDuplicated(names(funs))){
      stop("Multiple functions are specified with the same name", call. = FALSE)
    }
    if (replace & length(funs)!=1){
      stop("replace is TRUE but not one function is specified", call. = FALSE)
    }
    byvars <- names(select_vars_(names(x), by))
    if (!length(by)){
        byvars <- NULL
    }
    vars <- lazyeval::all_dots(vars)
    vars <- colwise_(x, funs_(funs), vars, byvars = byvars, replace = replace)
    setmutate_(x, vars, i, by)
}



colwise_ <- function(tbl, calls, vars, byvars = NULL, replace = FALSE) {
  vars <- select_vars_(tbl_vars(tbl), vars, exclude = byvars)
  if (!length(vars)){
    vars <- setdiff(names(tbl), byvars)
  }
  out <- vector("list", length(vars) * length(calls))
  dim(out) <- c(length(vars), length(calls))
  for (i in seq_along(vars)) {
    for (j in seq_along(calls)) {
      out[[i, j]] <- lazyeval::interp(calls[[j]],
        .values = list(. = as.name(vars[i])))
    }
  }
  dim(out) <- NULL  
  # modification is here:
  if (length(calls) == 1 & replace) {
    names(out) <- names(vars)
  } else {
    grid <- expand.grid(var = names(vars), call = names(calls))
    names(out) <- paste(grid$var, grid$call, sep = "_")
  }
  out
}






print_all <- function(x){
  print(x, nrow(x))
}

#set = function(x, new = NULL, fun = NULL, old = NULL, i = TRUE, by = NULL){
#    call <- substitute(set_(x, new = substitute(new), fun = func , old = substitute(old), i = ic, by =# substitute(by)), list(func = substitute(fun), ic = substitute(i)))
#    eval(call)
#}
#
#set_ = function(x, new = NULL , fun = NULL, old = NULL, i = TRUE, by = NULL){
#    i = substitute(i)
#    fun = substitute(fun)
#    if (is.null(old)){
#        newvars <- names(select_vars_(names(x), new))
#        vvars <- newvars
#    } else{
#        newvars <- new
#        vvars <- names(select_vars_(names(x), old))
#    }
#    byvars <- names(select_vars_(names(x), by))
#    if (!length(by)){
#        by <- NULL
#    }
#    if (is.call(fun)){
#      fun <- interp(fun, .values = list(. = as.name("x")))
#      fun <- substitute(function(x){y}, list(y = fun))
#    }
#    if (is.null(fun)){
#        call <- substitute(dt[, (newvars) := NULL])
#    } else{
#            if (length(byvars)){
#                call <- substitute(dt[i, new := lapply(.SD, fun), by = by, .SDcols= v], list(i = i, #fun = fun, v = vvars, by = byvars, new = newvars))
#            } else{
#                call <- substitute(dt[i, new := lapply(.SD, fun), .SDcols= v], list(i = i, fun = fun, #v = vvars, by = byvars, new = newvars))
#            }
#        }
#    dt_env <- dt_env(x, parent.frame())
#    eval(call, dt_env)
#}







