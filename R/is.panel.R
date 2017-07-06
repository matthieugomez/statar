#' Check whether a data.frame is a panel
#' 
#' @param x a data frame
#' @param ... a time variable
#' @param .dots Used to work around non standard evaluation
#' @return The function \code{is.panel} check that there are no duplicate combinations of the variables in ... and that no observation is missing for the last variable in ... (the time variable). 
#' @examples
#' library(dplyr)
#' df <- data_frame(
#'     id1    = c(1, 1, 1, 2, 2),
#'     id2   = 1:5,
#'     year  = c(1991, 1993, NA, 1992, 1992),
#'     value = c(4.1, 4.5, 3.3, 3.2, 5.2)
#' )
#' df %>% group_by(id1) %>% is.panel(year)
#' df1 <- df %>% filter(!is.na(year))
#' df1 %>% is.panel(year)
#' df1 %>% group_by(id1) %>% is.panel(year)
#' df1 %>% group_by(id1, id2) %>% is.panel(year)
#' @export
is.panel <- function(x, ..., .dots){
    byvars <- dplyr::group_vars(x)
    timevar <- setdiff(names(dplyr::select_vars(names(x), ...)), byvars)
    if (length(timevar) > 1) {
        message("There should only be one variable for time")
    }
    out <- TRUE
    if (anyNA(x[[timevar]])){
        ans <- which(is.na(x[[timevar]]))
        message(paste0("Variable ", timevar, " has missing values in ", length(ans)," row(s): ", paste(as.character(ans),collapse = ",")))
        out <- FALSE
    }
    overall = x %>% group_by(..., add = TRUE) %>% group_indices()
    if (anyDuplicated(overall)){
        groups <- split(seq_along(overall), overall)
        groups <- groups[vapply(groups, length, integer(1)) > 1]
        str <- vapply(groups, function(x) paste0("(", paste0(x, collapse = ","), ")"),
             character(1))
        message(paste0("Variables (", paste(c(byvars, timevar), collapse = " , "), ") have duplicates for rows ", paste(str, collapse = ", ")))
        out <- FALSE
    }
    out
}















