#' @export
gather_.data.table <- function(data, key_col, value_col, gather_cols,
                               na.rm = FALSE) {
  print("ok")
  data2 <- data.table::melt(data, measure.vars = gather_cols,
    variable.name = key_col, value.name = value_col, na.rm = na.rm)
  data2
}

#' @export
gather_.tbl_dt <- function(data, key_col, value_col, gather_cols,
                           na.rm = FALSE, convert = FALSE) {
  dplyr::tbl_dt(NextMethod())
}