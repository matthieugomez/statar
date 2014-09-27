#' Create unique names within environment
#' 
#' @param prefix A character that specifies prefix for new name
#' @param where A list or an environment
#' @param inherits  Should the name unique also in the enclosing frames of the environment?
#' @export
tempname=function(prefix, where, inherits=TRUE) {
    i <- 0L
    name <- prefix
    while (exists(name, where = where, inherits = inherits)) {
        i <- i + 1L
        name <- paste0(prefix, as.character(i))
    }
    name
}


tempname_list=function(prefix, l) {
    i <- 0L
    name <- prefix
    while (name %in% l) {
        i <- i + 1L
        name <- paste0(prefix, as.character(i))
    }
    name
}





