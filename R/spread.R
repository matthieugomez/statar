#' @export
spread_.data.table <- function(data, key_col, value_col, fill = NA, convert = FALSE) {
  id <- setdiff(names(data), c(key_col, value_col))
  length_lhs <- length(id)
  if (!length_lhs) {
  	id <- tempname("temp", data)
  	data[, (id) := 1:.N] 
  }
  else{
  	if (anyDuplicated(data, by = c(id))) stop("Duplicate identifiers for rows")
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


tempname=function(prefix, where, inherits=TRUE) {
    i <- 0L
    name <- prefix
    while (exists(name, where = where, inherits = inherits)) {
        i <- i + 1L
        name <- paste0(prefix, as.character(i))
    }
    name
}