#' Group multiple variable (similar to Stata group)
#'
#' @param ... vectors
#' @return An integer vector representing groups
#' @examples 
#' library(dplyr)                   
#' mutate(iris, g = group(Species))
# returns NA for groups where some variable is missing
#' mutate(iris, g = group(Species, floor(Sepal.Width)), na.rm = TRUE)
#' @export
group <- function(..., na.rm = FALSE){
  df <- data.frame(list(...))
  if (na.rm){
  	out <- rep(NA, nrow(df))
  	complete <- complete.cases(df)
  	indices <- df %>% filter(complete) %>% group_indices_(.dots = names(df))
  	out[complete] <- indices
  } else{
  	out <- group_indices_(df, .dots = names(df))
  }
  out
}
