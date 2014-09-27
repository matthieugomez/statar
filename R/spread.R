#' @export
spread_.data.table <- function(data, key_col, value_col, fill = NA, convert = FALSE) {
  response <- setdiff(names(data), c(key_col, value_col))
  length_lhs <- length(response)
  if (!length_lhs) {
  	response <- tempname("temp", data)
  	data[, (response) := 1:.N] 
  }
  else{
  	if (anyDuplicated(data, by = c(response))) stop("Duplicate identifiers for rows")
  	}
  formula <- reformulate(termlabels = key_col , response = response)
  data2 <- dcast.data.table(data, formula, value.var = value_col, fill = fill)
  if (!length_lhs) {
  	data[, (response) := NULL]
  	data2[, (response) := NULL]
  }
  if (convert) {
     data2[, names(data2) := lapply(.SD,type.convert, as.is = TRUE), .SDcols=names(data2)]
   }
  data2
}


#' @export
spread_.tbl_dt <- function(data, key_col, value_col, fill = NA,
                           convert = FALSE) {
  dplyr::tbl_dt(NextMethod())
}