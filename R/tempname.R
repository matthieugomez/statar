#' Create unique names within a list, a data.frame, or an environment
#'
#' @param prefix A character vector that specifies prefix for new name
#' @param where A chracter vector, list or an environment
#' @param inherits  Should the name unique also in the enclosing frames of the environment?
#' @param n An integar that specifies length of the output
#' @examples
#' tempname(c("temp1", "temp3"), 4)
#' tempname(globalenv())
#' tempname(data.frame(temp = 1), n = 3)
#' @export
tempname=function(where = globalenv() , n = 1, prefix = ".temp", inherits=TRUE) {
    all_names <- NULL
    i <- 0L
    name <- prefix
    while (n>0){
        i <- i + 1L
        if (is.character(where)){
            while (name %in% where) {
                name <- paste0(prefix, as.character(i))
                i <- i + 1L
            }
        all_names <- c(all_names, name)
        name <- paste0(prefix, as.character(i))
        n <- n-1
        } else{
            while (exists(name, where = where, inherits = inherits)){
            	   name <- paste0(prefix, as.character(i))
                   i <- i + 1L
        	}
            all_names <- c(all_names, name)
            name <- paste0(prefix, as.character(i))
            n <- n-1
        }
    }
    all_names
}



#get("id", pos = DT)

#Using data.table x inside a program can be cumbersom, the following steps are needed :
#- Create a string that does not  exist in names(x) 
#- eval(substitute() at every line, with a list composed of all the temporary variables created
#- Since `eval(substitute())` does not replace LHS in DT[, list(id = sum(v1)], I have to either #modify the call  or use setnames after the command
#
#There may be things I do wrongly, but all these steps makes programming cumbersome. So I have #thought of the following convenient functions:
#
#- a function assign_var (dt,v) that creates a binding from v to a symbol (corresponding to a #name that does not exist in data.table)
#- A function evaldt, that captures the argument, and replace every name bounded to a symbol by #the symbol. Compared to eval(substitute()) it automatically generates the list constituted #of variables that are binded to symbols, and it also replaces the lhs of a list (for #instance v in list(v="ok")).
#
#So this is how it works fora function that filter a condition within each group
#````
#rm(list =ls())
#N <- 2e6
#DT <- data.table(
#  id = 1:N,
#  v1 =  sample(5, N, TRUE),                          # int in range [1,5]
#  v2 =  sample(1e6, N, TRUE),                        # int in range [1,1e6]
#  v3 =  sample(round(runif(100,max=100),4), N, TRUE) # numeric e.g. 23.5749
#)
#I
#f <- function(DT,condition, by ){
# # assignnments
#  condition = substitute(condition)
#  by = substitute(by) 
#  assign_var(DT,tag)
## same command than in interactive mode, except evaldt in front of it
# evaldt(DT[DT[, list(tag = .I[condition]), by = by ]$tag])
#}
#
#I guess my this is neither directly a pull or a feature request, but I was looking for general #opinion on whether this is a good  way to use data.table within a function
#
#The code of assgn_var is below functions is below 
#````R
#tempname=function(prefix = "temp", where = globalenv() , inherits=TRUE, list = NULL) {
#    i <- 0L
#    out <- NULL
#    name <- prefix
#    if (!is.null(list)){
#        while (name %in% l) {
#            i <- i + 1L
#            name <- paste0(prefix, as.character(i))
#            }
#    } else{
#        while (exists(name, where = where, inherits = inherits)){
#            i <- i + 1L
#           name <- paste0(prefix, as.character(i))
#       }
#    }
#    name
#}
#
#assign_var <- function(x, ..., env = parent.frame(), inherits = FALSE){
#    names <-   as.character(substitute(list(...))[-1])
#    assign_var_(x = x, names = names, inherits = inherits, env = parent.frame())
#}
#
#assign_var_ <- function(x, names, env = parent.frame(), inherits=TRUE) {
#    for (name in names){
#        tempname <- paste("temp",name,sep="_")
#        assign(name, as.name(tempname), envir = env)
#    }
#}
#
#evaldt <- function(x, env = parent.frame()){
#    x <- substitute(x)
#    names <- ls(all.names = TRUE, envir = env)
#    L <- NULL
#    names_l <- NULL
#    for (name in names){
#        if (exists(name, env, inherits = FALSE, mode = "language") | exists(name, env, inherits #= FALSE, mode = "symbol")) {
#            L <- c(L, get(name, envir = env))
#            names_l <- c(names_l, name)
#        }
#    }
#    names(L) <- names_l
#    # replace names, even in LHS of list
#    replace_name <- function(x, lenv){
#        i <- 0
#        if ((is.atomic(x)) | (is.symbol(x))){
#            call <- substitute(substitute(y, lenv), list(y = x))
#            return(eval(call))
#        }
#        else{
#            out <- NULL
#            for (i in 1:length(x)){
#                if (!is.null(x[[i]])){
#                    x[[i]] <- replace_name(x[[i]], lenv)
#                }
#            }
#            names <- NULL
#            if (x[[1]] == quote(list)){
#                for (name in names(x)){
#                    if ((name!="") && (exists(name, lenv))) {
#                            names <- c(names, as.character(get(name, pos = lenv)))
#                    } 
#                    else{
#                        names <- c(names, name)
#                    }
#                }
#                names(x) <- names
#            }
#        }
#        x
#    }
#    call <- replace_name(x, as.list(L))
#    eval(call, env)
#}
#````




