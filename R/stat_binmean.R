#' Plot the mean of y over the mean of x within bins of x.
#'
#' @param n number of x-bins. Default to 20. Set to zero if you want to use distinct value of x for grouping.
#' @param na.rm If \code{FALSE} (the default), removes missing values with
#'    a warning.  If \code{TRUE} silently removes missing values.
#' @inheritParams ggplot2::stat_identity
#' @return a data.frame with additional columns:
#'   \item{xtile}{bins for x}
#'   \item{x}{mean of x}
#'   \item{y}{mean of y}
#' @examples
#' library(ggplot2)
#' g <- ggplot(iris, aes(x = Sepal.Width , y = Sepal.Length)) 
#' g + stat_binmean(n = 10)
#' g + stat_binmean(n = 10) + stat_smooth(method = "lm", se = FALSE)
#' g + stat_binmean(n = 0) 
#' g <- ggplot(iris, aes(x = Sepal.Width , y = Sepal.Length, color = Species))
#' g + stat_binmean(n = 10)
#' g + stat_binmean(n = 10) + stat_smooth(method = "lm", se = FALSE)
#' @export
stat_binmean <- function (mapping = NULL, data = NULL, geom = "point", position = "identity", show.legend = NA, inherit.aes = TRUE, na.rm = FALSE, n = 20, ...) {
  layer(
      data = data,
      mapping = mapping,
      stat = StatBinmean,
      geom = geom,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, n = n, ...)
    )
   }     
 
StatBinmean <-  ggproto("StatBinmean", Stat, 
  required_aes = c("x", "y"), 
  compute_group = function(data, scales, na.rm = FALSE, n = 20) {
    # Compute bins accross groups
    if (n == 0){
      # n = 0 : use values of x as group variables
      data <- data %>% dplyr::mutate(binx = x) 
    }
    else{
      # n > 0: bin x in n categories
      if ("weight" %in% names(data)){
        data <- data %>% dplyr::mutate(binx = xtile(x, n = n, w = weight)) 
      }
      else{
        data <- data %>% dplyr::mutate(binx = ntile(x, n)) 
      }
   }

   # compute mean within (group, binx)
   data <- data %>% dplyr::group_by(group, binx)
    if ("weight" %in% names(data)){
      data <- data %>% dplyr::mutate(x = weighted.mean(x, w = w, na.rm = na.rm), y = weighted.mean(y, w = w, na.rm = na.rm))
    }
    else{
      data <-  data %>% dplyr::mutate(x = mean(x, na.rm = na.rm), y = mean(y, na.rm = na.rm)) 
    }
    data %>%  dplyr::slice(1) %>% ungroup() %>% dplyr::filter(!is.na(binx))
  }
)
