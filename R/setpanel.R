#' Check panel data
#' 
#' @param x a vector or matrix
#' @param ... id variables. Default to none
#' @param along_with Date variable.
#' @param vars Used to work around non standard evaluation
#' @return The function checks no data variable is missing, that there are no duplicates for (id, along_with), and if both these conditions are satisfied, resort the data.table in place along the id and time variables.
#' @export
setpanel <- function(x, ..., along_with = NULL){
    setpanel_(x, vars = lazy_dots(...) , along_with = substitute(along_with))
}

#' @export
#' @rdname  setpanel
setpanel_ <- function(x, vars, along_with = NULL){
    byvars <- names(select_vars_(names(x), vars))
    along_with  <- names(select_vars_(names(x), along_with, exclude = byvars))
    if (anyNA(x[[along_with]])){
        ans <- which(is.na(x[[along_with]]))
        stop(paste(along_with, "has missing values in", length(ans),"rows:", paste(as.character(ans),collapse = ",")), call. = FALSE)
    }
    if (anyDuplicated(x, by = c(byvars, along_with))){
        overall <- dplyr::id(x[, c(byvars, along_with), with = FALSE])
        groups <- split(seq_along(overall), overall)
        groups <- groups[vapply(groups, length, integer(1)) > 1]
        str <- vapply(groups, function(x) paste0("(", paste0(x, collapse = ", "), ")"),
             character(1))
        stop("Duplicate (id, time) for rows ", paste(str, collapse = ", "),
             call. = FALSE)
    }
    setkeyv(x, c(byvars, along_with))
}








