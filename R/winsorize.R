#' Winsorize a numeric vector
#' 
#' @param x A vector of values
#' @param cutpoints Cutpoints under and above which are defined outliers. Default is (median - five times interquartile range, median + five times interquartile range). Compared to bottom and top percentile, this takes into account the whole distribution of the vector.
#' @param probs A vector of probabilities that can be used instead of cutpoints. Quantiles are computed as the inverse of the empirical distribution function (type = 1)
#' @param replace Values by which outliers are replaced. Default to cutpoints. A frequent alternative is NA.
#' @param verbose Boolean. Should the percentage of replaced values printed?
#' @examples                          
#' v <- c(1:4, 99)
#' winsorize(v)
#' winsorize(v, replace = NA)
#' winsorize(v, probs = c(0.01, 0.99))
#' winsorize(v, cutpoints = c(1, 50))
#' @export
winsorize <- function(x, probs = NULL, cutpoints = NULL , replace = c(cutpoints[1], cutpoints[2]), verbose = TRUE){
  dummy = is.integer(x)
  if (!is.null(probs)){
      stopifnot(is.null(cutpoints))
      stopifnot(length(probs)==2)
      cutpoints <- stats::quantile(x, probs, type = 1, na.rm = TRUE)
  } else if (is.null(cutpoints)){
      l <- stats::quantile(x, c(0.25, 0.50, 0.75), type = 1, na.rm = TRUE) 
      cutpoints <- c(l[2]-5*(l[3]-l[1]), l[2]+5*(l[3]-l[1]))
  } else{
      stopifnot(length(cutpoints)==2)
  }
  if (is.integer(x)) cutpoints <- round(cutpoints)
  bottom <- x < cutpoints[1]
  top <- x > cutpoints[2]
  if (verbose){
    length <- length(x)
    message(paste(sprintf("%3.2f", 100*sum(bottom, na.rm = TRUE)/length),"% observations replaced at the bottom"))
    message(paste(sprintf("%3.2f", 100*sum(top, na.rm = TRUE)/length),"% observations replaced at the top"))
  }
  x[bottom] <- replace[1]
  x[top] <- replace[2]
  if (dummy){
    x <- as.integer(x)
  }
  x
}

#' @export
#' @rdname winsorize
winsorise <- winsorize