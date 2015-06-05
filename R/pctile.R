#' Weighted quantile of type 2 (similar to Stata _pctile)
#'
#' @param x A vector
#' @param probs A vector of probabilities
#' @param w A weight vector
#' @param na.rm Should missing values be returned?
#' @export
pctile <- function(x, probs = c(0.25, 0.5, 0.75), w = NULL, na.rm = FALSE){
  if (is.null(w)){
    quantile(x = x, type = 2, probs = probs, na.rm = na.rm)
  } else{
      if (anyNA(x) | anyNA(w)) {
        if (na.rm) {
          na <- is.na(x) | is.na(w)
          x <- x[!na]
          w <- w[!na]
        }
        else{
          stop("Missing values not allowed when na.rm is FALSE", call. = FALSE)
        } 
      }
      # Ensure x and w in ascending order of x
      order <- order(x)
      cumsum <- cumsum(w[order])
      n <- cumsum[length(cumsum)]
      # follow definition of quantile 2 
      index <- n * probs
      j <- floor(index)
      low <- x[order[pmin(length(x),   .bincode(j, c(-Inf, cumsum)))]]
      high <- x[order[pmin(length(x),   .bincode(j + 1, c(-Inf, cumsum)))]]
      ifelse(j == index, 0.5 * low + 0.5 * high, high)
    }
}
