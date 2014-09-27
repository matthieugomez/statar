#' @export
spread_.data.table <- function(data, key_col, value_col, fill = NA, convert = FALSE) {
  formula <- reformulate(termlabels = value_col , response = key_col)
  data2 <- dcast.data.table(data, formula)
  if (convert) {
     data2[] <- lapply(data2, type.convert, as.is = TRUE)
   }
  data2
}


#' @export
spread_.tbl_dt <- function(data, key_col, value_col, fill = NA,
                           convert = FALSE) {
  dplyr::tbl_dt(NextMethod())
}