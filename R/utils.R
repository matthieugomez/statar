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


fquantile <- function(x, probs, w = NULL){
  if (is.null(w)){
    quantile(x = x, type = 3, probs = probs)
  } else{
    wtd.quantile(x = x, type = "i/n", weights = w, probs = probs)
  }
}
# fast functions not exported
#fquantile <- function(x, probs, na.rm = TRUE, w = NULL){
#  l_na <- sum(is.na(x))
#  if (!na.rm & l_na){
#    return(rep(NA, probs))
#  } else{
#    if (is.null(w)){
#      order <- forderv(x)
#      if (!length(order)){
#        order <- seq_along(x)
#      }
#      index <- pmax(1, ceiling(probs * (length(x)-l_na)))
#      return(x[order[l_na + index]])
#    } else{
#      take <- !is.na(x) & !is.na(w)
#      w <- w[take]
#      w <- as.numeric(w)
#      w <- w / sum(w)
#      x <- x[take]
#      order <- forderv(x)
#      if (!length(order)){
#        order <- seq_along(x)
#      }
#      cum <- cumsum(w[order])
#      index <- pmin(length(x), .bincode(probs, c(-Inf, cum)))
#      return(x[order[index]])
#    }
#  }
#}



#ffquantile <- function (x, probs = seq(0, 1, 0.25), names = TRUE, 
#    type = 7, ...) 
#{
#    if (is.factor(x)) {
#        if (!is.ordered(x) || !type %in% c(1L, 3L)) 
#            stop("factors are not allowed")
#        lx <- levels(x)
#    }
#    else lx <- NULL
#    eps <- 100 * .Machine$double.eps
#    if (any((p.ok <- !is.na(probs)) & (probs < -eps | probs > 
#        1 + eps))) 
#        stop("'probs' outside [0,1]")
#    n_na <- sum(is.na(x))
#    n <- length(x)-n_na
#    if (na.p <- any(!p.ok)) {
#        o.pr <- probs
#        probs <- probs[p.ok]
#        probs <- pmax(0, pmin(1, probs))
#    }
#    np <- length(probs)
#    if (n > 0 && np > 0) {
#        order <- data.table:::forderv(x)
#
#        if (type == 7) {
#            index <- 1 + (n - 1) * probs
#            lo <- floor(index)
#            hi <- ceiling(index)
#            qs <- x[order[n_na + lo]]
#            i <- which(index > lo)
#            h <- (index - lo)[i]
#            qs[i] <- (1 - h) * qs[i] + h * x[order[n_na + hi[i]]]
#        }
#        else {
#            if (type <= 3) {
#                nppm <- if (type == 3) 
#                  n * probs - 0.5
#                else n * probs
#                j <- floor(nppm)
#                h <- switch(type, (nppm > j), ((nppm > j) + 1)/2, 
#                  (nppm != j) | ((j%%2L) == 1L))
#            }
#            else {
#                switch(type - 3, {
#                  a <- 0
#                  b <- 1
#                }, a <- b <- 0.5, a <- b <- 0, a <- b <- 1, a <- b <- 1/3, 
#                  a <- b <- 3/8)
#                fuzz <- 4 * .Machine$double.eps
#                nppm <- a + probs * (n + 1 - a - b)
#                j <- floor(nppm + fuzz)
#                h <- nppm - j
#                if (any(sml <- abs(h) < fuzz)) 
#                  h[sml] <- 0
#            }
#            order <- c(order[n_na + 1], order[n_na + 1], order, order[n_na + n], order[n_na + n])
#            qs <- x[order[n_na + j + 2L]]
#            qs[h == 1] <- x[order[n_na + j + 3L]][h == 1]
#            other <- (0 < h) & (h < 1)
#            if (any(other)) 
#                qs[other] <- ((1 - h) * x[order[n_na + j + 2L]] + h * x[order[n_na + j + 
#                 3L]])[other]
#            }
#    }
#    else {
#        qs <- rep(NA_real_, np)
#    }
#    if (is.character(lx)) 
#        qs <- factor(qs, levels = seq_along(lx), labels = lx, 
#            ordered = TRUE)
#    if (names && np > 0L) {
#        dig <- max(2L, getOption("digits"))
#        names(qs) <- paste0(if (np < 100) 
#            formatC(100 * probs, format = "fg", width = 1, digits = dig)
#        else format(100 * probs, trim = TRUE, digits = dig), 
#            "%")
#    }
#    if (na.p) {
#        o.pr[p.ok] <- qs
#        names(o.pr) <- rep("", length(o.pr))
#        names(o.pr)[p.ok] <- names(qs)
#        o.pr
#    }
#    else qs
#}



# funique not faster than unique




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







