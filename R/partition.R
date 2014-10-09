#' Create quantile categories (corresponds to Stata command xtile)
#'
#' @param x A vector
#' @param cutpoints Cutpoints to use when \code{nq} is not specified.  For instance \code{cutpoints = 0.4} creates two groups, one for observations equal or below 0.4, one for observations superior to 0.4. Cutpoints must be unique.
#' @param prob A vector of probabilities that an be used instead of cutpoints. Quantiles are computed as the inverse of the empirical distribution function (type = 1)
#' @param n_quantiles A numeric specifying number of quantiles. Can be used instead of cutpoints
#' @return An integer vector representing groups separated by cutpoints. Includes missing values when present in the original vector.
#' @examples 
#' v <- c(NA, 1:10)                   
#' partition(v, n_quantiles = 3) # 3 groups based on terciles
#' partition(v, prob = c(0.3, 0.7)) # 3 groups based on terciles
#' partition(v, cutpoints = c(2, 3)) # 3 groups based on two cutpoints
#' @export
partition <- function(x, cutpoints = NULL, prob = NULL, n_quantiles = NULL){
  if (!is.null(n_quantiles)){
  if (!is.null(cutpoints)|!is.null(prob)) stop("Only one option among cutpoints, prob and n_quantiles can be used")
      breaks <- tail(head(quantile(x, seq(0, 1, length = n_quantiles + 1), type = 1, na.rm = TRUE), -1),-1)
      return(partition(x, cutpoints = breaks))
  } else if (!is.null(prob)){
	  	if (!is.null(cutpoints)) stop("Only one option among cutpoints, prob and n_quantiles can be used")
	  	breaks <- tail(head(quantile(x, prob, type = 1, na.rm = TRUE), -1),-1)
	}
  breaks <- c(min(min(x, na.rm = TRUE),min(cutpoints)) -1, cutpoints , max(max(x, na.rm = TRUE), max(cutpoints)) + 1)
  if (anyDuplicated(breaks)) stop("Cutpoints are not unique")
  cut(x, breaks = breaks , labels = FALSE, include.lowest=TRUE)
}

