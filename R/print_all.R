#' prints whole data.table
#'
#' @param x a data.table
#' @export


print_all <- function(x){
	print(x, nrow(x))
}