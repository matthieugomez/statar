#' Group multiple variable (similar to Stata group)
#'
#' @param ... vectors
#' @param na.rm  Should groups where some variable is NA return NA? Default to FALSE
#' @return An integer vector representing groups
#' @examples 
#' library(dplyr)                   
#' mutate(iris, g = group(Species))
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
