#' Move variables to the front (Stata command order)
#' 
#' @param DT A tbl_dt or tbl_grouped_dt.
#' @param ... Variables to include/exclude in s You can use same specifications as in select. If missing, defaults to all non-grouping variables.
#' @param inplace Change data.table in place or not
#' @examples
#' library(data.table)
#' library(dplyr)
#' N <- 100; K <- 10
#' DT <- data.table(
#'   id = 1:N,
#'   v1 =  sample(5, N, TRUE),                          
#'   v2 =  sample(1e6, N, TRUE),                       
#'   v3 =  sample(round(runif(100, max = 100), 4), N, TRUE) 
#' )
#' DT  %>% col_order(starts_with("v"), inplace = TRUE)
#' @export
col_order <- function(.data, ..., inplace = FALSE) {
  colorder_(.data, vars = lazyeval::lazy_dots(...) , inplace = inplace)
}
#' @export
col_order_ <- function(.data, vars, inplace = FALSE ) {
  if (length(vars) == 0) {
     vars <- lazyeval::lazy_dots(everything())
   }
  vars <- select_vars_(tbl_vars(.data), vars)
  if (!inplace) .data <- copy(.data)
  setcolorder(.data,c(vars,setdiff(names(.data),vars)))
  .data
}


