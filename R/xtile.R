#' Bin variable in groups (similar to Stata xtile)
#'
#' @param x A vector
#' @param cutpoints Cutpoints to use when \code{nq} is not specified.  For instance \code{cutpoints = 0.4} creates two groups, one for observations equal or below 0.4, one for observations superior to 0.4. 
#' @param probs A vector of probabilities that an be used instead of cutpoints. Quantiles are computed as the inverse of the empirical distribution function (type = 1)
#' @param n A numeric specifying number of quantiles. Can be used instead of cutpoints
#' @param wt A variable specifying weight in case the option n_quantiles is specified.
#' @return An integer vector representing groups corresponding to cutpoints. Includes missing values when present in the original vector.
#' @examples 
#' x <- c(NA, 1:10)                   
#' xtile(x, n = 3) # 3 groups based on terciles
#' xtile(x, probs = c(0.3, 0.7)) # 3 groups based on two quantiles
#' xtile(x, cutpoints = c(2, 3)) # 3 groups based on two cutpoints
#' @export
xtile <- function(x, n = NULL, probs = NULL, cutpoints = NULL, wt = NULL){
  if (!is.null(n)){
    probs <- seq(1/n, 1-1/n, length = n -1)
  }
  if (!is.null(probs)){
    cutpoints <- pctile(x, probs, wt = wt, na.rm = TRUE)
  }
  # In stata xtile is (-infty, x_p1], (xp1, xpe], like .bincode 
  .bincode(x, c(-Inf, cutpoints , +Inf) , include.lowest = TRUE)
}
