#' @export
spread_.data.table <- function(data, key_col, value_col, fill = NA, convert = FALSE) {
  response <- setdiff(names(data), c(key_col, value_col))
  length_lhs <- length(response)
  if (!length_lhs) {
  	response <- tempname("temp",data)
  	data[,(response):=.N] 
  }
  formula <- reformulate(termlabels = key_col , response = response)
  data2 <- dcast.data.table(data, formula, fill = fill, value.var = value_col)
  if (!length_lhs) {
  	data[,(response):=NULL]
  }
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