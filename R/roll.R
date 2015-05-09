#' Apply rolling functions within time intervals
#' 
#' @param x a vector or matrix
#' @param FUN function to apply on \code{x}
#' @param n a numeric specifying the rolling window
#' @param order_by override the default ordering to use another vector
#' @param along_with  use this variable to roll the function based on the  \code{[along_with - n, along_with]} rather than the past \code{n} rows. NA are not accepted
#' @param closed Logical of length 2 (recycled) Should interval be closed ? Default to c(TRUE, TRUE)
#' @param min An integer. Return NA if the number of observations is strictkly lower than min
#' @param ... options to pass to the function \code{FUN}
#' @examples
#' date  = c(1, 2, 4, 7)
#' x <- c(1, 1, 1, 1)
#' roll_lag(x,sum, n = 1, along_with = date)
#' roll_lag(x,sum, n = 1, along_with = date, min = 2)
#' roll_lag(x, sum, n = 1, along_with = date)
#' roll_lag(x, sum, n = 2, along_with = date)
#' roll_lead(x, sum, n = 1, along_with = date)
#' roll_lead(x, sum, n = 2, along_with = date)
#' y <- c(1, 2, 1, 1)
#' roll_lag(list(x,y), function(z){cov(z[[1]], z[[2]])},  n = 2, along_with = date)
#' @export
#' @aliases roll_lag roll_lead
#' @rdname roll


#' @rdname roll
roll_lag <- function(x, FUN, n, along_with = NULL, order_by = NULL, closed = c(TRUE, TRUE), min = 1L, ...){
    if (length(closed)==1){
        closed = rep(closed,2)
    }
    FUN <- match.fun(FUN)
     if (!is.null(order_by) | !is.null(along_with)){
        if (!is.null(order_by)){
            sort <- order_by
        } else{
            sort <- along_with
        }
        l <- length(sort)
        ord <- order(sort)
        sort <- sort[ord]
        if (is.matrix(x) | is.data.frame(x)){
            x <- x[ord,, drop = FALSE]
        } else if (is.list(x)){
            x <- lapply(x,function(z){z[ord]})
        } else{
            x <- x[ord]
        }
        undo <- match(seq_len(l), ord)
    } else{
        if (!is.null(nrow(x))){
            l <- nrow(x)
        } else{
            l <- length(x)
        }
    }
    seq = seq_len(l)
    if (!is.null(along_with)){
        if (anyNA(sort)){
            stop(sort, "contains NA")
        }
        if (closed[[1]]){
            f_start <- .bincode(sort-n, c(-Inf, sort))
        } else{
            sort <- as.double(sort)
            f_start <- findInterval(sort - n, c(-Inf, sort))
        } 
        if (closed[[2]]){
            vec <- lapply(seq, function(i) f_start[i]:i)
        } else{
            seq[seq <=  f_start] <- NA
            vec <- lapply(seq, function(i) if (!is.na(i)) (f_start[i]:(i-1)))
        }
    } else {
        if (closed[[1]]){
            f_start <- pmax(seq - n, 1)
        } else{
            f_start <- pmax(seq - n + 1, 1)
        } 
        if (closed[[2]]){
            vec <- lapply(seq, function(i) f_start[i]:i)
        } else{
            seq[seq <=  f_start] <- NA
            vec <- lapply(seq, function(i) if (!is.na(i)) f_start[i]:(i-1))
        }
    }
    if (is.list(x)){
        out <- lapply(vec, function(v){
            if (length(v) >= min){
                x <- lapply(x,function(z){z[v]})
                FUN(x,...)
            } else NA
            })
    } else if (is.matrix(x)|is.data.frame(x)){
        out <- lapply(vec, function(v){
            if (length(v) >= min){
                FUN(x[v,, drop = FALSE],...)
            } else NA
        })
    }
     else{
        out <- lapply(vec, function(v){
            if (length(v) >= min){
                   FUN(x[v],...)
               } else NA
           })
    }
    out <- simplify2array(out, higher = TRUE)
    if (!is.null(order_by) | !is.null(along_with)){
        out[undo]
    } else{
        out
    }
}



#' @export
#' @rdname roll
roll_lead <- function(x, FUN, n, along_with = NULL, order_by = NULL, closed = c(TRUE, TRUE), min = 1L,...){
    if (length(closed)==1){
        closed = rep(closed,2)
    }
    FUN <- match.fun(FUN)
     if (!is.null(order_by) | !is.null(along_with)){
        if (!is.null(order_by)){
            sort <- order_by
        } else{
            sort <- along_with
        }
        l <- length(sort)
        ord <- order(sort)
        undo <- match(seq_len(l), ord)
        if (is.matrix(x) | is.data.frame(x)){
            x <- x[ord,, drop = FALSE]
        } else if (is.list(x)){
            x <- lapply(x,function(z){z[ord]})
        } else{
            x <- x[ord]
        }
        sort <- sort[ord]
    } else{
        if (!is.null(nrow(x))){
            l <- nrow(x)
        } else{
            l <- length(x)
        }
    }
    seq = seq_len(l)
    if (!is.null(along_with)){
        if (anyNA(sort)){
            stop(sort, "contains NA")
        }
       if (closed[[2]]){
            sort <- as.double(sort)
            f_end <- findInterval(sort, sort - n)
          } else{
            f_end <- .bincode(sort, c(sort-n, Inf))
          } 
          if (closed[[1]]){
              vec <- lapply(seq, function(i) i:f_end[i])
          } else{
              seq[seq >=  f_end] <- NA
              vec <- lapply(seq, function(i) if (!is.na(i)) (i+1):f_end[i])
          }
    } else {
         if (closed[[1]]){
             f_end <- pmin(seq + n, l)
         } else{
             f_end <- pmin(seq + n - 1, l)-1
         } 
         if (closed[[2]]){
            vec <- lapply(seq, function(i) i:f_end[i])
         } else{
            seq[seq > f_end] <- NA
             vec <- lapply(seq, function(i) if (!is.na(i)) (i+1):f_end[i])
         }
    }
    if (is.list(x)){
        out <- lapply(vec, function(v){
            if (length(v) >= min){
                x <- lapply(x,function(z){z[v]})
                FUN(x,...)
            } else NA
            })
    } else if (is.matrix(x)|is.data.frame(x)){
        out <- lapply(vec, function(v){
            if (length(v) > min){
                FUN(x[v,, drop = FALSE],...)
            } else NA
        })
    }
     else{
        out <- lapply(vec, function(v){
            if (length(v) >= min){
                   FUN(x[v],...)
               } else NA
           })
    }
    out <- simplify2array(out, higher = TRUE)
    if (!is.null(order_by) | !is.null(along_with)){
        out[undo]
    } else{
        out
    }
}







