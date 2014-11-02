#' Check panel data
#' 
#' @param x a vector or matrix
#' @param ... id variables. Last is time variable
#' @param vars Used to work around non standard evaluation
#' @return The function checks no data variable is missing, that there are no duplicates for (id, along_with), and if both these conditions are satisfied, resort the data.table in place along the id and time variables.
#' @export
setpanel <- function(x, ...){
    setpanel_(x, vars = lazy_dots(...))
}

#' @export
#' @rdname  setpanel
setpanel_ <- function(x, vars){
    vars <- names(select_vars_(names(x), vars))
    along_with  <- vars[length(vars)]
    vars <- setdiff(vars, along_with)
    if (anyNA(x[[along_with]])){
        ans <- which(is.na(x[[along_with]]))
        stop(paste0("Variable ", along_with, " has missing values in ", length(ans)," row(s): ", paste(as.character(ans),collapse = ",")), call. = FALSE)
    }
    if (anyDuplicated(x, by = c(vars, along_with))){
        overall <- dplyr::id(x[, c(vars, along_with), with = FALSE])
        groups <- split(seq_along(overall), overall)
        groups <- groups[vapply(groups, length, integer(1)) > 1]
        str <- vapply(groups, function(x) paste0("(", paste0(x, collapse = ","), ")"),
             character(1))
        stop(paste0("Variables (", paste(c(vars, along_with), collapse = " , "), ") have duplicates for rows ", paste(str, collapse = ", ")), call. = FALSE)
    }
    setkeyv(x, c(vars, along_with))
}








