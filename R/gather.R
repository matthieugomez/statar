#' @export
gather_.data.table <- function(data, key_col, value_col, gather_cols,
                               na.rm = FALSE, convert = FALSE) {
  data2 <- data.table::melt(data, measure.vars = gather_cols,
    variable.name = key_col, value.name = value_col, na.rm = na.rm)
  if (convert) {
    data2[[key_col]] <- type.convert(as.character(data2[[key_col]]),
      as.is = TRUE)
  }
  data2
}

#' @export
gather_.tbl_dt <- function(data, key_col, value_col, gather_cols,
                           na.rm = FALSE, convert = FALSE) {
  dplyr::tbl_dt(NextMethod())
}