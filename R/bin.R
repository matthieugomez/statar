#' Bin a numeric vector and return integer codes for the binning (corresponds to Stata command xtile)
#'
#' @param x A vector
#' @param cutpoints Cutpoints to use when \code{nq} is not specified.  For instance \code{cutpoints = 0.4} creates two groups, one for observations equal or below 0.4, one for observations superior to 0.4. 
#' @param probs A vector of probabilities that an be used instead of cutpoints. Quantiles are computed as the inverse of the empirical distribution function (type = 1)
#' @param n_quantiles A numeric specifying number of quantiles. Can be used instead of cutpoints
#' @param w A variable specifying weight in case the option n_quantiles is specified.
#' @return An integer vector representing groups corresponding to cutpoints. Includes missing values when present in the original vector.
#' @examples 
#' x <- c(NA, 1:10)                   
#' bin(x, n = 3) # 3 groups based on terciles
#' bin(x, probs = c(0.3, 0.7)) # 3 groups based on two quantiles
#' bin(x, cutpoints = c(2, 3)) # 3 groups based on two cutpoints
#' @export
bin <- function(x, n = NULL, probs = NULL, cutpoints = NULL, w = NULL){
  if (!is.null(n)){
    probs <-  seq(1/n, 1-1/n, length = n -1)
  }
  if (!is.null(probs)){
    if (is.null(w)){
      # compute quantile and bin in one pass by reordering first
      order <- data.table:::forderv(x)
      l_na <- sum(is.na(x))
      l_probs <- length(probs)
      l_x <- length(x)-l_na
      f <- c(0, ceiling(seq_len(l_probs+1)*l_x/(l_probs+1)))
      aux <- rep(c(NA, seq_len(l_probs + 1)), times = c(l_na, diff(f)))
      aux[order] <- aux
      return(aux)
    } else{
      cutpoints <- wtd.quantile(x, probs, type ="i/n", na.rm = TRUE, weights = w)
    }
  }
  .bincode(x, c(-Inf, cutpoints , +Inf) , include.lowest=TRUE)
}




#oldbin <- function(x, n = NULL, probs = NULL, cutpoints = NULL, w = NULL){
#  if (!is.null(n)){
#    probs <-  seq(1/n, 1-1/n, length = n -1)
#  }
#  if (!is.null(probs)){
#    if (is.null(w)){
#     cutpoints <- quantile(x, probs, type =1, na.rm = TRUE)
#    } else{
#      cutpoints <- wtd.quantile(x, probs, type ="i/n", na.rm = TRUE, weights = w)
#    }
#  }
#  .bincode(x, c(-Inf, cutpoints , +Inf) , include.lowest=TRUE)
#}
#
#
#midbin <- function(x, n = NULL, probs = NULL, cutpoints = NULL, w = NULL){
#  if (!is.null(n)){
#    probs <-  seq(1/n, 1-1/n, length = n -1)
#  }
#  if (!is.null(probs)){
#    if (is.null(w)){
#     cutpoints <- fquantile(x, probs, na.rm = TRUE)
#    } else{
#      cutpoints <- wtd.quantile(x, probs, type ="i/n", na.rm = TRUE, weights = w)
#    }
#  }
#  .bincode(x, c(-Inf, cutpoints , +Inf) , include.lowest=TRUE)
#}



fquantile <- function (x, probs = seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, 
    type = 7, ...) 
{
    if (is.factor(x)) {
        if (!is.ordered(x) || !type %in% c(1L, 3L)) 
            stop("factors are not allowed")
        lx <- levels(x)
    }
    else lx <- NULL
    if (na.rm) 
        x <- x[!is.na(x)]
    else if (anyNA(x)) 
        stop("missing values and NaN's not allowed if 'na.rm' is FALSE")
    eps <- 100 * .Machine$double.eps
    if (any((p.ok <- !is.na(probs)) & (probs < -eps | probs > 
        1 + eps))) 
        stop("'probs' outside [0,1]")
    n <- length(x)
    if (na.p <- any(!p.ok)) {
        o.pr <- probs
        probs <- probs[p.ok]
        probs <- pmax(0, pmin(1, probs))
    }
    np <- length(probs)
    if (n > 0 && np > 0) {
        if (type == 7) {
            index <- 1 + (n - 1) * probs
            lo <- floor(index)
            hi <- ceiling(index)
            x <- x[data.table:::forderv(x)] 
            qs <- x[lo]
            i <- which(index > lo)
            h <- (index - lo)[i]
            qs[i] <- (1 - h) * qs[i] + h * x[hi[i]]
        }
        else {
            if (type <= 3) {
                nppm <- if (type == 3) 
                  n * probs - 0.5
                else n * probs
                j <- floor(nppm)
                h <- switch(type, (nppm > j), ((nppm > j) + 1)/2, 
                  (nppm != j) | ((j%%2L) == 1L))
            }
            else {
                switch(type - 3, {
                  a <- 0
                  b <- 1
                }, a <- b <- 0.5, a <- b <- 0, a <- b <- 1, a <- b <- 1/3, 
                  a <- b <- 3/8)
                fuzz <- 4 * .Machine$double.eps
                nppm <- a + probs * (n + 1 - a - b)
                j <- floor(nppm + fuzz)
                h <- nppm - j
                if (any(sml <- abs(h) < fuzz)) 
                  h[sml] <- 0
            }
            x <- x[data.table:::forderv(x)] 
            x <- c(x[1L], x[1L], x, x[n], x[n])
            qs <- x[j + 2L]
            qs[h == 1] <- x[j + 3L][h == 1]
            other <- (0 < h) & (h < 1)
            if (any(other)) 
                qs[other] <- ((1 - h) * x[j + 2L] + h * x[j + 
                  3L])[other]
        }
    }
    else {
        qs <- rep(NA_real_, np)
    }
    if (is.character(lx)) 
        qs <- factor(qs, levels = seq_along(lx), labels = lx, 
            ordered = TRUE)
    if (names && np > 0L) {
        dig <- max(2L, getOption("digits"))
        names(qs) <- paste0(if (np < 100) 
            formatC(100 * probs, format = "fg", width = 1, digits = dig)
        else format(100 * probs, trim = TRUE, digits = dig), 
            "%")
    }
    if (na.p) {
        o.pr[p.ok] <- qs
        names(o.pr) <- rep("", length(o.pr))
        names(o.pr)[p.ok] <- names(qs)
        o.pr
    }
    else qs
}


fsort <- function(x, partial, n){
  x[order(x[partial])]
}
