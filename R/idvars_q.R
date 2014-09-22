
idvars_q=function(id,l){
    if (is.call(id) && id[[1L]] == "-") {
        idm = TRUE
        id = as.character(id[[2L]])
    } else {
        idm = FALSE
        id <- as.character(id)
    }
    if (!is.character(id)) stop("ids should be column numbers or names")
    if (any(is.na(id))) stop("Some items of ids are NA)")
    if (any(!id %chin%  l)){
        #try wildcard 
        if (length(id) !=1L) stop("Some items of ids are not column names")
        id_vector <- strsplit(id, "\\s+")[[1]]
        id=NULL
         for (c in id_vector){
            temp <- grep(glob2rx(c),l,value=TRUE)
            if (!length(temp)) stop("Some items of ids are not column names")
            id <- c(id,temp)
        }
    }
    if (idm) idvars = setdiff(l, id) else idvars = id
    return(idvars)
}
 

