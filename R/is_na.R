#' Returns boolean vector corresponding to rows with one missing observation in variable specified in "by"
#'
#' lead and lag are useful for comparing values for date offset by a constant
#' @param x a data.table
#' @param by a character or numeric vector for columns
#' @examples
#' library(data.table)
#' DT <- data.table( v1 = c(NA, 1), v2 = c(1, NA))
#' is_na(DT, by = "v1")
#' is_na(DT, by = "v2")
#' is_na(DT)
#' @export
is_na <- function(x, by = seq_along(x)){
      if (is.character(by)) {
          old = by
          by = chmatch(by, names(x), nomatch = 0L)
          if (any(by == 0L)) 
              stop("Columns ", paste(old[by == 0L], collapse = ","), 
                  " doesn't exist in the input data.table")
      }
      data.table:::is_na(x, by)
}


