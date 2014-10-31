#' Apply rollling functions
#' 
#' @param x a vector
#' @param FUN function to apply
#' @param n an integer. 
#' @param order_by override the default ordering to use another vector
#' @param along_with  use this variable to roll the function based on the  \code{[along_with - n, along_with]} rather than past \code{n} rows. 
#' @param along_with Vector of numeric for dates. NA are not accepted
#' @param ... options for the function
#' @examples
#' along_with  = c(1, 2, 4, 7)
#' x = c(1, 1, 1, 1)
#' roll_lag(x, sum, n = 1, along_with = along_with)
#' roll_lag(x, sum, n = 2, along_with = along_with)
#' roll_lead(x, sum, n = 1, along_with = along_with)
#' roll_lead(x, sum, n = 2, along_with = along_with)
roll_lag <- function(x, FUN, n, along_with = NULL, order_by = NULL, ...){
     if (!is.null(order_by)){
       return(with_order(order_by, roll, x, FUN = FUN, n = n, ...))
    } else if (!is.null(along_with)){
        l <- length(along_with)
        ord <- order(along_with)
        undo <- match(seq_len(l), ord)
        x <- x[ord]
        along_with <- along_with[ord]
        f_start <- l- findInterval(n-along_with, -rev(along_with)) + 1
        f_end <- seq_len(l)
        out <- mapply(function(start, end){
            FUN(x[start:end],...)}, f_start, f_end)
        return(out[undo])
    } else{
        end <- seq_along(x)
        start <- pmax(end-n,0)

        return(mapply(function(start, end){
            index <- start:end
            FUN(x[index],...)
        }, start, end))
    }
}


roll_lead <- function(x, FUN, n, along_with = NULL,order_by = NULL, ...){
     if (!is.null(order_by)){
       return(with_order(order_by, roll, x, FUN = FUN, n = n, ...))
    } else if (!is.null(along_with)){
        l <- length(along_with)
        ord <- order(along_with)
        undo <- match(seq_len(l), ord)
        x <- x[ord]
        along_with <- along_with[ord]
 
        f_start <- seq_len(l)
        f_end <- findInterval(along_with, along_with - n)
        out <- mapply(function(start, end){
            FUN(x[start:end],...)}, f_start, f_end)
        return(out[undo])
    } else{

        start <- pmax(end+n,0)
        end <- pmax(x+n,0)
        return(mapply(function(start, end){
            index <- start:end
            FUN(x[index],...)
        }, start, end))
    }
}


