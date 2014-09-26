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





