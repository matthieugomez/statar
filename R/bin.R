#' Bin a numeric vector and return integer codes for the binning (corresponds to Stata command xtile)
#'
#' @param x A vector
#' @param cutpoints Cutpoints to use when \code{nq} is not specified.  For instance \code{cutpoints = 0.4} creates two groups, one for observations equal or below 0.4, one for observations superior to 0.4. 
#' @param probs A vector of probabilities that an be used instead of cutpoints. Quantiles are computed as the inverse of the empirical distribution function (type = 1)
#' @param n_quantiles A numeric specifying number of quantiles. Can be used instead of cutpoints
#' @param w A variable specifying weight in case the option n_quantiles is specified.
#' @return An integer vector representing groups corresponding to cutpoints. Includes missing values when present in the original vector.
#' @examples 
#' v <- c(NA, 1:10)                   
#' bin(v, n_quantiles = 3) # 3 groups based on terciles
#' bin(v, probs = c(0.3, 0.7)) # 3 groups based on two quantiles
#' bin(v, cutpoints = c(2, 3)) # 3 groups based on two cutpoints
#' @export
bin <- function(x, cutpoints = NULL, probs = NULL, n_quantiles = NULL, w = NULL){
  if (!is.null(n_quantiles)){
  if (!is.null(cutpoints)|!is.null(probs)) stop("Only one option among cutpoints, probs and n_quantiles can be used")
      if (is.null(w)){
        cutpoints <- tail(head(quantile(x, seq(0, 1, length = n_quantiles + 1), type = 1, na.rm = TRUE), -1),-1)
      } else{
        cutpoints <- tail(head(wtd.quantile(x, seq(0, 1, length = n_quantiles + 1), type ="i/n", na.rm = TRUE, weights = w), -1),-1)
      }
  } else if (!is.null(probs)){
	  	if (!is.null(cutpoints)) stop("Only one option among cutpoints, probs and n_quantiles can be used")
	  cutpoints <- quantile(x, probs, type = 1, na.rm = TRUE)
	}
  breaks <- c(min(min(x, na.rm = TRUE),min(cutpoints)) -1, cutpoints , max(max(x, na.rm = TRUE), max(cutpoints)) + 1)
  if (anyDuplicated(breaks)) warning("Cutpoints are not unique", call. = FALSE)
  .bincode(x, breaks = breaks , include.lowest=TRUE)
}

