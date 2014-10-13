#' @export
spread_.data.table <- function(data, key_col, value_col, fill = NA, convert = FALSE) {
  id <- setdiff(names(data), c(key_col, value_col))
  length_lhs <- length(id)
  if (!length_lhs) {
  	id <- tempname(data)
  	data[, (id) := 1:.N] 
    on.exit(data[, (id) := NULL])
  }
  else{
  	if (anyDuplicated(data, by = c(id))) stop("Duplicate identifiers", call. = FALSE)
  	}
  formula <- reformulate(termlabels = key_col , response = id)
  data2 <- dcast.data.table(data, formula, value.var = value_col, fill = fill)
  if (!length_lhs) {
  	data[, (id) := NULL]
  	data2[, (id) := NULL]
  }
  if (convert) {
     data2[, names(data2) := lapply(.SD,type.convert, as.is = TRUE), .SDcols = names(data2)]
   }
  data2
}

#' @export
spread_.tbl_dt <- function(data, key_col, value_col, fill = NA,
                           convert = FALSE) {
  dplyr::tbl_dt(NextMethod())
}

#' @export
spread_.grouped_dt  <- function(.data, ..., .dots) {
  dplyr::tbl_dt(NextMethod(), copy = FALSE)
}

