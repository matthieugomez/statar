tempvar=function(l) {
    maxTries <- 1e+06
    ii <- 0L
    while (ii < maxTries) {
        name =paste0("temp",as.character(ii))
        if (!name %in% l) {
            return(name)
        }
        ii <- ii + 1L
    }
}