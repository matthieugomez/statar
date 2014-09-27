#' @export
spread_.data.table <- function(data, key_col, value_col) {
  formula <- reformulate(termlabels = value_col , response = key_col)
  data2 <- data.table.dcast(DT, formula)
  data2
}


#' @export
spread_.tbl_dt <- function(data, key_col, value_col, fill = NA,
                           convert = FALSE) {
  dplyr::tbl_dt(NextMethod())
}