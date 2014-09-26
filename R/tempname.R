tempname=function(prefix, where, inherits=TRUE) {
    i <- 0L
    name <- prefix
    while (exists(name, where = where, inherits = inherits)) {
        i <- i + 1L
        name <- paste0(prefix, as.character(i))
    }
    name
}


