#' Apply rollling functions
#' 
#' @param x a vector
#' @param FUN function to apply
#' @param n an integer. Window is all the observations in \code{[along_with - n, along_with]} 
#' @param along_with Integer variable specifying dates. NA are not accepted
#' @param ... options for the function
#' @examples
#' along_with  = c(1, 2, 4, 7)
#' x = c(1, 1, 1, 1)
#' roll(x, sum, n = 1, along_with = along_with)
#' roll(x, sum, n = 2, along_with = along_with)
#' roll(x, sum, n = -1, along_with = along_with)
#' roll(x, sum, n = -2, along_with = along_with)
#' DT[, a := roll(value, mean, n = 2, order_by = date, na.rm = TRUE), by = id]
roll <- function(x, FUN, n, along_with = NULL,order_by = NULL, ...){
     if (!is.null(order_by)){
       return(with_order(order_by, roll, x, FUN = FUN, n = n, ...))
    } else if (!is.null(along_with)){
        l <- length(along_with)
        ord <- order(along_with)
        undo <- match(seq_len(l), ord)
        x <- x[ord]
        along_with <- along_with[ord]
        if (n>0){
            f_start <- l- findInterval(n-along_with, -rev(along_with)) + 1
            f_end <- seq_len(l)
        } else {
            f_start <- seq_len(l)
            f_end <- findInterval(along_with, along_with + n)
        }
        out <- mapply(function(start, end){
            FUN(x[start:end],...)}, f_start, f_end)
        return(out[undo])
    } else{
        if (n>0){
            end <- seq_along(x)
            start <- pmax(end-n,0)
        } else{
            start <- pmax(end-n,0)
            end <- pmax(x-n,0)
        }
        return(mapply(function(start, end){
            index <- start:end
            FUN(x[index],...)
        }, start, end))
    }
}


along_with = c(1, 8,9,21)