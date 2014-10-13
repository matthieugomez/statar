dt_env <- function(dt, env) {
  env <- new.env(parent = env, size = 2L)
  env$dt <- dt
  env$vars <- deparse_all(groups(dt))
  env
}

deparse_all <- function(x) {
  deparse2 <- function(x) paste(deparse(x, width.cutoff = 500L), collapse = "")
  vapply(x, deparse2, FUN.VALUE = character(1))
}



#' @export
assign_var <- function(x, ..., env = parent.frame(), inherits = FALSE){
    names <- sapply(lazy_dots(...), function(x){as.character(x$expr)})
    assign_var_(x = x, names = names, inherits = inherits, env = parent.frame())
}
#' @export
assign_var_ <- function(x, names, env = parent.frame(), inherits=TRUE) {
    for (name in names){
        tempname <- tempname(paste("temp",name,sep="_"), where = env, inherits = inherits)
        assign(name, tempname, env)
    }
}

#' @export
evaldt <- function(x, env = parent.frame()){
    x <- substitute(x)
    names <- ls(all.names = TRUE, envir = env)
    L <- NULL
    names_l <- NULL
    for (name in names){
        if (exists(name, envir = env, inherits = FALSE, mode = "character")){
            get_name <- get(name, envir = env)
            # suppress NULL but also character vector because I'm not sure how to replace list(a,.x) when length(.x) is >1
            if (length(get_name)==1){
                L <- c(L, as.name(get_name))
                names_l <- c(names_l, name)
            }
        }
    }
    names(L) <- names_l
    # replace names, even in LHS of list
    replace_name <- function(x, lenv, env){
        i <- 0
        if (is.atomic(x) | is.symbol(x)){
            if (str_detect(as.character(x),"*\\.")){
                xx <- str_replace(as.character(x),".","")
                if (exists(xx, envir = env, inherits = FALSE, mode = "character")){
                    get_name <- get(xx, envir = env)
                    if (length(get_name)==1){
                      return(as.name(get_name))
                    }
                } else{
                  return(x)
                }
            } else{
                return(x)
            }
        }
        else{
            out <- NULL
            for (i in 1:length(x)){
                if (!is.null(x[[i]])){
                    x[[i]] <- replace_name(x[[i]], lenv, env)
                }
            }
            names <- NULL
            if (x[[1]] == quote(list)){
                for (name in names(x)){
                    if (str_detect(name,"\\.")) {
                        namename <- str_replace(as.character(name),".","")
                        if (exists(namename, envir = env, inherits = FALSE, mode = "character")){
                            names <- c(names, get(namename, env = env))
                        } else{
                            names <- c(names, name)
                        }
                    } 
                    else{
                        names <- c(names, name)
                    }
                }
                names(x) <- names
            }
        }
        x
    }
    call <- replace_name(x, as.list(L), env = env)
    eval(call, env)
}

#' @export
setv = function(x, new = names(x) , f, v = new, i = TRUE, by = NULL){
  f = substitute(f)
  if (is.call(f)){
      f <- interp(f, .values = list(. = as.name("x")))
    }
    fun <- substitute(function(x){y}, list(y = f))
    i <- substitute(i)
    call <- substitute(dt[i, (new) := lapply(.SD, fun), by = (by), .SDcols= v], list(i = i, fun = fun, v = v, by = by, new = new))
    dt_env <- dt_env(x, parent.frame())
    eval(call, dt_env)
}