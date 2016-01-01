#' Check whether a data.frame is a panel
#' 
#' @param df a data frame
#' @param ... a list of variables. All except last are the id variable. Last is time variable
#' @param vars Used to work around non standard evaluation
#' @return The function \code{is.panel} check that there are no duplicate combinations of the variables in ... and that no observation is missing for the last variable in ... (the time variable). 
#' @examples
#' library(dplyr)
#' df <- data_frame(
#'     id1    = c(1, 1, 1, 2, 2),
#'     id2   = 1:5,
#'     year  = c(1991, 1993, NA, 1992, 1992),
#'     value = c(4.1, 4.5, 3.3, 3.2, 5.2)
#' )
#' is.panel(df, id1, year)
#' df <- df %>% filter(!is.na(year))
#' is.panel(df, id1, year)
#' is.panel(df, id1, id2, year)
#' @export
is.panel <- function(df, ...){
    is.panel_(df, vars = lazy_dots(...))
}

#' @export
#' @rdname  is.panel
is.panel_ <- function(df, vars){
    charvars <- names(select_vars_(names(df), vars))
    timevar  <- charvars[length(charvars)]
    idvars <- setdiff(charvars, timevar)
    out <- TRUE
    if (anyNA(df[[timevar]])){
        ans <- which(is.na(df[[timevar]]))
        message(paste0("Variable ", timevar, " has missing values in ", length(ans)," row(s): ", paste(as.character(ans),collapse = ",")))
        out <- FALSE
    }
    overall = group_indices_(df, .dots = vars)
    if (anyDuplicated(overall)){
        groups <- split(seq_along(overall), overall)
        groups <- groups[vapply(groups, length, integer(1)) > 1]
        str <- vapply(groups, function(x) paste0("(", paste0(x, collapse = ","), ")"),
             character(1))
        message(paste0("Variables (", paste(charvars, collapse = " , "), ") have duplicates for rows ", paste(str, collapse = ", ")))
        out <- FALSE
    }
    out
}















