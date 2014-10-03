#' Winsorize a numeric vector
#' 
#' @param x A vector of values
#' @param cutpoints Cutpoints to defined outliers. Default is (median - five times interquartile range, median + five times interquartile range). Compared to bottom and top percentile, this takes into account the whole distribution of the vector. This also makes windorize idempotent in most of cases \code{winsorize(winsorize(x)) == winsorize(x)}.
#' @param replace Values by which outliers are replaced. Default to cutpoints. A frequent alternative is NA.
#' @examples                          
#' v <- c(1, 4, 6, 99)                      
#' winsorize(v)
#' winsorize(v, replace = NA)
#' winsorize(v, cutpoints = quantile(v, c(0.01, 0.99), na.rm = TRUE))
#' @name winsorize
NULL


#' @export
#' @rdname winsorize
winsorize <- function(x, 
                      cutpoints = {
                        l <- quantile(v, c(0.25, 0.50, 0.75), type = 1, na.rm = TRUE) 
                        c(l[2]-5*(l[3]-l[1]), l[2]+5*(l[3]-l[1]))
                      }, 
                      replace = c(cutpoints[1], cutpoints[2])){
  if (!length(cutpoints)==2) stop("cutpoints must be a vector of length 2")
  x[x < cutpoints[1]] <- replace[1]
  x[x > cutpoints[2]] <- replace[2]
  x
}

#' @export
#' @rdname winsorize
winsorise <- function(x, 
                      cutpoints = {
                        l <- quantile(v, c(0.25, 0.50, 0.75), type = 1, na.rm = TRUE) 
                        c(l[2]-5*(l[3]-l[1]), l[2]+5*(l[3]-l[1]))
                      }, 
                      replace = c(cutpoints[1],cutpoints[2])){
    winsorize(x, cutpoints, replace)
}
