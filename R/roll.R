#' Apply rollling functions
#' 
#' @param x a vector or matrix
#' @param FUN function to apply on \code{x}
#' @param n an integer specifying the rolling window
#' @param order_by override the default ordering to use another vector
#' @param along_with  use this variable to roll the function based on the  \code{[along_with - n, along_with]} rather than past \code{n} rows. NA are not accepted
#' @param ... options to pass to the function \code{FUN}
#' @examples
#' along_with  = c(1, 2, 4, 7)
#' x = c(1, 1, 1, 1)
#' roll_lag(x,sum, n = 1, along_with = along_with)
#' roll_lag(x, sum, n = 1, along_with = along_with)
#' roll_lag(x, sum, n = 2, along_with = along_with)
#' roll_lead(x, sum, n = 1, along_with = along_with)
#' roll_lead(x, sum, n = 2, along_with = along_with)
#' roll_lag(list(x,y), function(z){cor(z[[1]], z[[2]])},  n = 1, along_with = along_with)
#' @export
#' @aliases roll_lag roll_lead
#' @rdname roll


#' @rdname roll
roll_lag <- function(x, FUN, n, along_with = NULL, order_by = NULL, closed.left = TRUE, ...){
     if (!is.null(order_by)){
       return(with_order(order_by, roll_lag, x, FUN = FUN, n = n, ...))
    } else if (!is.null(along_with)){
        l <- length(along_with)
        ord <- order(along_with)
        undo <- match(seq_len(l), ord)
        if (is.matrix(x)){
            x <- x[ord,, drop = FALSE]
        } else if (is.list(x)){
            x <- lapply(x,function(z){z[ord]})
        } else{
            x <- x[ord]
        }
        along_with <- along_with[ord]
        if (closed.left){
            f_start <- .bincode(along_with-n, c(-Inf, along_with))
        } else{
            f_start <- findInterval(along_with-n, c(-Inf, along_with))
        } 
        f_end <- seq_len(l)
        if (is.list(x)){
            out <- mapply(function(start, end){
                x <- lapply(x,function(z){z[seq.int(start,end)]})
                FUN(x,...)
                }, 
            f_start, f_end)
        } else if (is.matrix(x)){
            out <- mapply(function(start, end){
                FUN(x[seq.int(start,end),, drop = FALSE],...)
                }, 
            f_start, f_end)
        }
         else{
            out <- mapply(function(start, end){
                FUN(x[seq.int(start,end)],...)
            }, 
            f_start, f_end)
        }
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



#' @rdname roll
roll_lead <- function(x, FUN, n, along_with = NULL, order_by = NULL, closed.left = TRUE, ...){
     if (!is.null(order_by)){
       return(with_order(order_by, roll_lag, x, FUN = FUN, n = n, ...))
    } else if (!is.null(along_with)){
        l <- length(along_with)
        ord <- order(along_with)
        undo <- match(seq_len(l), ord)
        if (is.matrix(x)){
            x <- x[ord,, drop = FALSE]
        } else if (is.list(x)){
            x <- lapply(x,function(z){z[ord]})
        } else{
            x <- x[ord]
        }
        along_with <- along_with[ord]
        if (closed.right){
                f_end <- findInterval(along_with, along_with - n)
        } else{
                 f_end <- .bincode(along_with, c(along_with-n, Inf))
        } 
        f_end <- seq_len(l)
        if (is.list(x)){
            out <- mapply(function(start, end){
                x <- lapply(x,function(z){z[seq.int(start,end)]})
                FUN(x,...)
                }, 
            f_start, f_end)
        } else if (is.matrix(x)){
            out <- mapply(function(start, end){
                FUN(x[seq.int(start,end),, drop = FALSE],...)
                }, 
            f_start, f_end)
        }
         else{
            out <- mapply(function(start, end){
                FUN(x[seq.int(start,end)],...)
            }, 
            f_start, f_end)
        }
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



#roll_lag <- function(x, n, along_with = NULL, order_by = NULL, closed.left = TRUE){
#    roll_lag_(lazy(x),  n, along_with = along_with, order_by =  order_by, closed.left = closed.left)
#}
#
#roll_lag_ <- function(vars, n, along_with = NULL, order_by = NULL, closed.left = TRUE){
#     if (!is.null(order_by)){
#       return(with_order(order_by, roll_lag_, vars, n = n, closed.left = TRUE))
#    } else{
#        names <- all.vars(vars$expr)
#        print(vars)
#        env <- common_env(vars)
#        print(env)
#        print(ls(env))
#        names_right <- names[length(get(names, env))==length(along_with)]
#        m <- sapply(names_right, function(x){get(x, env)})
#        values <- lapply(seq_along(names_right), function(x){substitute(m[, x], list(x = x))})
#        expr <- interp(vars$expr, .values = setNames(values, names_right))
#        if (!is.null(along_with)){
#        l <- length(along_with)
#        ord <- order(along_with)
#        undo <- match(seq_len(l), ord)
#        x <- x[ord]
#        along_with <- along_with[ord]
#        if (closed.left){
#            f_start <- .bincode(along_with-n, c(-Inf, along_with))
#        } else{
#            f_start <- findInterval(along_with-n, c(-Inf, along_with))
#        } 
#        f_end <- seq_len(l)
#        out <- mapply(
#            function(start, end){
#                env$m <- m[seq.int(start,end),, drop = FALSE]
#                eval(expr, env)
#            },
#            f_start, f_end)
#        return(out[undo])
#    } else{
#        end <- seq_along(x)
#        start <- pmax(end-n,0)
#        return(mapply(function(start, end){
#            index <- start:end
#            FUN(x[index],...)
#        }, start, end))
#    }
#    }
#}


