#' Check whether data.table is a panel data
#' 
#' @param x a vector or matrix
#' @param ... id variables. Last is time variable
#' @param vars Used to work around non standard evaluation
#' @return The function \code{is.panel} checks no data variable is missing and that there are no duplicates for (id, along_with). The function \code{setpanel} reorders data.table in place if \code{is.panel} is \code{TRUE}
#' @examples
#' library(data.table)
#' DT <- data.table(
#'     id    = c(1, 1, 1, 2, 2),
#'     year  = c(1991, 1993, NA, 1992, 1992),
#'     value = c(4.1, 4.5, 3.3, 3.2, 5.2)
#' )
#' is.panel(DT, id, year)
#' DT <- na.omit(DT, cols = "year")
#' is.panel(DT, id, year)
#' DT <- unique(DT, by = c("id", "year"))
#' is.panel(DT, id, year)
#' setpanel(DT, id, year)
#' @aliases  setpanel
#' @export
is.panel <- function(x, ...){
    is.panel_(x, vars = lazy_dots(...))
}

#' @export
#' @rdname  is.panel
is.panel_ <- function(x, vars){
    vars <- names(select_vars_(names(x), vars))
    along_with  <- vars[length(vars)]
    vars <- setdiff(vars, along_with)
    out <- TRUE
    if (anyNA(x[[along_with]])){
        ans <- which(is.na(x[[along_with]]))
        message(paste0("Variable ", along_with, " has missing values in ", length(ans)," row(s): ", paste(as.character(ans),collapse = ",")))
        out <- FALSE
    }
    if (anyDuplicated(x, by = c(vars, along_with))){
        overall <- dplyr::id(x[, c(vars, along_with), with = FALSE])
        groups <- split(seq_along(overall), overall)
        groups <- groups[vapply(groups, length, integer(1)) > 1]
        str <- vapply(groups, function(x) paste0("(", paste0(x, collapse = ","), ")"),
             character(1))
        message(paste0("Variables (", paste(c(vars, along_with), collapse = " , "), ") have duplicates for rows ", paste(str, collapse = ", ")))
        out <- FALSE
    }
    out

}




#' @export
#' @rdname is.panel
setpanel <- function(x, ...){
    setpanel_(x, vars = lazy_dots(...))
}

#' @export
#' @rdname  is.panel
setpanel_ <- function(x, vars){
    vars <- names(select_vars_(names(x), vars))
    if (is.panel_(x, vars)){
        along_with  <- vars[length(vars)]
        vars <- setdiff(vars, along_with)
        setkeyv(x, c(vars, along_with))
    }
}













