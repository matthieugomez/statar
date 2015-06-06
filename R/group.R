#' Group multiple variable (similar to Stata group)
#'
#' @param ... vectors
#' @return An integer vector representing groups
#' @examples 
#' library(dplyr)                   
#' mutate(iris, g = group(Species))
#' mutate(iris, g = group(Species, floor(Sepal.Width)))
#' @export
group <- function(...){
  df <- data.frame(..., stringsAsFactors = FALSE)
  df %>% group_by_(.dots = names(df)) %>% group_indices
}
