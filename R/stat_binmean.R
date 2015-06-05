#' Plot the mean of y over the mean of x in x bins
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("stat", "quantile")}
#'
#' @param n number of x-bins
#' @param na.rm If \code{FALSE} (the default), removes missing values with
#'    a warning.  If \code{TRUE} silently removes missing values.
#' @inheritParams ggplot2::stat_identity
#' @return a data.frame with additional columns:
#'   \item{xtile}{bins for x}
#'   \item{x}{mean of x}
#'   \item{y}{mean of y}
#' @examples
#' ggplot(iris, aes(x = Sepal.Width , y = Sepal.Length)) + stat_binmean()
#' ggplot(iris, aes(x = Sepal.Width , y = Sepal.Length, color = Species)) + stat_binmean(n=10) 
#' ggplot(iris, aes(x = Sepal.Width , y = Sepal.Length, color = Species)) + stat_binmean(n=10) + stat_smooth(method = "lm", se = FALSE)
#' @export
stat_binmean <- function (mapping = NULL, data = NULL, geom = "point", position = "identity", n = 20, cutpoints = NULL,  na.rm = FALSE, ...) {
  Statbinmean$new(mapping = mapping, data = data, geom = geom, position = position, n = n, cutpoints = cutpoints, na.rm = na.rm, ...)
}

Statbinmean <- proto(getFromNamespace("Stat", "ggplot2"), {
    objname <- "binmean"
    calculate <- function(., data, scales, n = 20, cutpoints = curpoints, na.rm = FALSE, ...) {
      if (is.null(data$weight)){
        out <- data %>% dplyr::mutate(xbin = ntile(x, n))  %>% dplyr::group_by(xbin, add = TRUE)  %>% dplyr::mutate(x = mean(x, na.rm = na.rm), y = mean(y, na.rm = na.rm)) 
      }
      else{
        out <- data %>% stata::mutate(xbin = xtile(x, n = n, w = weight))  %>% dplyr::group_by(xbin, add = TRUE)  %>% dplyr::mutate(x = weighted.mean(x, w = w, na.rm = na.rm), y = weighted.mean(y, w = w, na.rm = na.rm))
      }
      out %>%  dplyr::slice(1) %>% dplyr::filter(!is.na(xbin))
    }
    calculate_groups <- function(., data, scales, n = 20, na.rm = FALSE,
    ...) {
      return(calculate(., data %>%  dplyr::group_by(group), n = n, na.rm = na.rm, ...))
    }
    required_aes <- c("x", "y")
    default_geom <- function(.) GeomPoint
})
