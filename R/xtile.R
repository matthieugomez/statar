#' Create quantile categories (corresponds to Stata command xtile)
#'
#' @param x A vector
#' @param nq Number of quantiles. Quantiles are computed as the inverse of the empirical distribution function
#' @param cutpoints Cutpoints to use when \code{nq} is not specified.  For instance \code{cutpoints = 0.4} creates two groups, one for observations equal or below 0.4, one for observations superior to 0.4
#' @examples 
#' library(dplyr)
#' DT %>% group_by(id) %>% mutate(new = xtile(date, nq = 3))
#' DT %>% group_by(v1) %>% mutate(xtile(v2, nq = 3))
#' DT %>% group_by(v1) %>% mutate(xtile(v2, cutpoints = c(1e5,5e5) ))
#' @export
xtile <- function(x, nq = NULL, cutpoints = NULL){
  if (!is.null(nq)){
  if (!is.null(cutpoints)) stop("Only one option out of nq and cutpoints can be used")
      breaks <- tail(head(quantile(x, seq(0, 1, length = nq + 1), type = 1), -1),-1)
      return(xtile(x, cutpoints = breaks))
  } 
  length=length(cutpoints)+1
  cut(x, breaks = c(min(x), cutpoints ,max(x)) , labels = seq_len(length), include.lowest=TRUE)
}

