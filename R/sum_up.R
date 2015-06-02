#' Gives summary statistics (corresponds to Stata command summarize)
#' 
#' @param x a data.table
#' @param ... Variables to include. Defaults to all non-grouping variables. See the \link[dplyr]{select} documentation.
#' @param w Weights. Default to NULL. 
#' @param i Condition
#' @param by Groups within which summary statistics are printed. Default to NULL. See the \link[dplyr]{select} documentation.
#' @param d Should detailed summary statistics be printed?
#' @param digits Number of significant decimal digits. Default to 3
#' @param vars Used to work around non-standard evaluation.
#' @examples
#' library(data.table)
#' N <- 100
#' DT <- data.table(
#'   id = 1:N,
#'   v1 = sample(5, N, TRUE),
#'   v2 = sample(1e6, N, TRUE)
#' )
#' sum_up(DT)
#' sum_up(DT, v2, d = TRUE)
#' sum_up(DT, starts_with("v"), by = v1)
#' sum_up(DT, by = v1)
#' @export
sum_up <- function(x, ...,  d = FALSE, w = NULL,  i = NULL, by = NULL, digits = 3) {
  UseMethod("sum_up")
}

#' @export
#' @method sum_up default
sum_up.default <- function(x, ...,  d = FALSE, w = NULL, digits = 3) {
  xsub <- copy(deparse(substitute(x)))
  if (is.null(w)){
    x <- list(x)
    setnames(setDT(x), xsub)
    print(x)
    sum_up_(x, vars = xsub, d = d, digits = digits)
  } else{
    x <- list(x, w)
    setnames(setDT(x),  c(xsub, "weight"))
    sum_up_(x, vars = xsub, d = d, w = w, digits = digits)
  }
}

#' @export
#' @method sum_up data.table
sum_up.data.table <- function(x, ...,  d = FALSE, w = NULL,  i = NULL, by = NULL, digits = 3) {
  sum_up_(x, vars = lazy_dots(...) , d = d, w = substitute(w), i = substitute(i), by = substitute(by), digits = digits)
}


#' @export
#' @rdname sum_up
sum_up_<- function(x, vars, d = FALSE,  w= NULL,  i = NULL, by = NULL, digits = 3) {
  stopifnot(is.data.table(x))
  w <- names(select_vars_(names(x), w))
  if (!length(w)) w <- NULL
  byvars <- names(select_vars_(names(x), by))
  dots <- all_dots(vars)
  vars <- names(select_vars_(names(x), dots, exclude = c(w, byvars)))
  if (length(vars) == 0) {
     vars <- setdiff(names(x), c(byvars, w))
  }
  nums <- sapply(x, is.numeric)
  nums_name <- names(nums[nums==TRUE])
  vars <- intersect(vars,nums_name)
  if (!length(vars)) stop("Please select at least one non-numeric variable", call. = FALSE)
  if (!is.null(w)){
    w <- x[[which(names(x)== w)]]
  }
  if (!is.null(i)){
    x <- x[i, c(vars, w, byvars), with = FALSE]
  }
  if (!length(byvars)){
    out <- x[, describe(.SD, d = d, w = w), .SDcols = vars]
  } else{
    out <- x[, describe(.SD, d = d, w = w), by = byvars, .SDcols = vars]
  }
  setkeyv(out, c("variable", byvars))
  setcolorder(out, c("variable", byvars, setdiff(names(out), c("variable", byvars))))
  print_pretty_summary(out, digits = digits)
  invisible(out)
}



describe <- function(M, d = FALSE, w = NULL){
  names <- names(M)
  # Now starts the code 
  if (d==FALSE) {
    if (!is.null(w)){
      sum <- lapply(M ,function(x){
        take <- !is.na(x) & !is.na(w)
        x_omit <- x[take]
        w_omit <- w[take]
        m <- matrixStats::weightedMean(x_omit, w = w_omit)
        c(length(x_omit), length(x)-length(x_omit), m, sqrt(matrixStats::weightedMean((x_omit-m)^2, w = w_omit)), matrixStats::colRanges(x_omit, dim = c(length(x_omit), 1)))
      })
    }else{
      sum <- lapply(M ,function(x){
        x_omit <- na.omit(x)
      c(length(x_omit), length(x) - length(x_omit), mean(x_omit), sd(x_omit), matrixStats::colRanges(x_omit, dim = c(length(x_omit), 1)))
      })
    }
    setDT(sum)
    sum <- t(sum)
    sum <- as.data.table(sum)
    sum <- cbind(names, sum)
    setnames(sum, c("variable", "N","N_NA","mean","sd","min", "max"))
  } else {
    N <- nrow(M)
    f=function(x){
      if (!is.null(w)){
        take <- !is.na(x) & !is.na(w)
        x_omit <- x[take]
        w_omit <- w[take]
        m <- matrixStats::weightedMean(x_omit, w = w_omit)
        sum_higher <- matrixStats::colWeightedMeans(cbind((x_omit-m)^2,(x_omit-m)^3,(x_omit-m)^4), w = w_omit)
        sum_higher[1] <- sqrt(sum_higher[1])
        sum_higher[2] <- sum_higher[2]/sum_higher[1]^3
        sum_higher[3] <- sum_higher[3]/sum_higher[1]^4
        sum_quantile <- fquantile(x_omit, c(0, 0.01, 0.05, 0.1, 0.25, 0.50, 0.75, 0.9, 0.95, 0.99, 1), w = w_omit)
      } else{
        x_omit <- na.omit(x)
        m <-mean(x_omit)
        sum_higher <- colMeans(cbind((x_omit-m)^2,(x_omit-m)^3,(x_omit-m)^4))
        sum_higher[1] <- sqrt(sum_higher[1])
        sum_higher[2] <- sum_higher[2]/sum_higher[1]^3
        sum_higher[3] <- sum_higher[3]/sum_higher[1]^4
        sum_quantile= fquantile(x_omit, c(0, 0.01, 0.05, 0.1, 0.25, 0.50, 0.75, 0.9, 0.95, 0.99, 1))
      }
      n_NA <- length(x) - length(x_omit)
      sum <- c(N-n_NA, n_NA, m, sum_higher, sum_quantile)
    }
    sum <- mclapply(M, f)
    setDT(sum)
    sum <- t(sum)
    sum <- as.data.table(sum)
    sum <- cbind(names, sum)
    setnames(sum, c("variable", "N","N_NA","mean","sd","skewness","kurtosis","min","1%","5%","10%","25%","50%","75%","90%","95%","99%","max"))
  }
  sum
}



print_pretty_summary <- function(x, digits = 3){
 # f <- function(y){
 #   if (is.numeric(y)){
 #     y <- sapply(y, function(z){.iround(z, decimal.places = digits)})
 #     end <- paste0(paste(rep("0", digits), collapse = ""),"$")
 #     y <- str_replace(y,end,"")
 #     y[y==""] <- "0"
 #     y <- str_replace(y,"\\.$","")
 #     y <- str_replace(y,"^-0$","0")
 #   } 
 #   y
 # }
 # x <- x[, lapply(.SD, f), .SDcols = names(x)]
  if ("skewness" %in% names(x)){
    x1 <- select(x, -one_of(c("1%","5%","10%","25%","50%","75%","90%","95%","99%")))
    x2 <-  select(x, -one_of(c("N","N_NA","mean","sd","skewness","kurtosis", "min", "max")))
    stargazer(x1, type = "text", summary = FALSE, digits = digits, rownames = FALSE)
    stargazer(x2, type = "text", summary = FALSE, digits = digits, rownames = FALSE)
  } else{
  stargazer(x, type = "text", summary = FALSE, digits = digits, rownames = FALSE)
  }
}

