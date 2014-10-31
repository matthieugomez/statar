dt_env <- function(dt, env, byvars) {
  env <- new.env(parent = env, size = 2L)
  env$dt <- dt
  env$byvars <- byvars
  env
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







