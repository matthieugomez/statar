#' Gives summary statistics (corresponds to Stata command summarize)
#' 
#' @param x a data.frame
#' @param ... Variables to include. Defaults to all non-grouping variables. See the \link[dplyr]{select} documentation.
#' @param w Weights. Default to NULL. 
#' @param i Condition
#' @param d Should detailed summary statistics be printed?
#' @param .dots Used to work around non-standard evaluation.
#' @examples
#' library(dplyr)
#' N <- 100
#' df <- data_frame(
#'   id = 1:N,
#'   v1 = sample(5, N, TRUE),
#'   v2 = sample(1e6, N, TRUE)
#' )
#' sum_up(df)
#' sum_up(df, v2, d = TRUE)
#' sum_up(df, v2, d = TRUE, i = v1>3)
#' df %>% group_by(v1) %>% sum_up(starts_with("v"))
#' @return a data.frame 
#' @export
sum_up <- function(x, ...,  d = FALSE, w = NULL,  i = NULL) {
  UseMethod("sum_up")
}

#' @export
#' @method sum_up default
sum_up.default <- function(x, ...,  d = FALSE, w = NULL) {
  if (is.null(w)){
    x <- setNames(data_frame(x), "x")
  } else{
    x <- setNames(data_frame(x, w),  c("x", "weight"))
  }
  sum_up_(x, .dots = "x", d = d, w = w)
}



#' @export
#' @method sum_up data.frame
sum_up.data.frame <- function(x, ...,  d = FALSE, w = NULL,  i = NULL) {
  sum_up_(x, .dots = lazy_dots(...) , d = d, w = substitute(w), i = substitute(i))
}


#' @export
#' @rdname sum_up
sum_up_<- function(x, ..., .dots, d = FALSE,  w= NULL,  i = NULL) {
  w <- names(select_vars_(names(x), w))
  byvars <- as.character(groups(x))
  dots <- all_dots(.dots, ..., all_named = TRUE)
  vars <- select_vars_(names(x), dots, exclude = c(w, byvars))
  if (length(vars) == 0) {
     vars <- setdiff(names(x), c(byvars, w))
  }
  nums <- sapply(x, is.numeric)
  nums_name <- names(nums[nums == TRUE])
  vars <- intersect(vars, nums_name)
  if (!length(vars)) stop("Please select at least one non-numeric variable", call. = FALSE)
  newname = NULL
  if (!is.null(i)){
      newname <- tempname(x, 1)
      x <- mutate_(x, .dots = setNames(list(interp(~ as.integer(i), i = i)), newname))
      if (length(w)){
        x <- mutate_(x, .dots = setNames(list(interp(~ w*newname, w = as.name(w), newname = as.name(newname))), newname))
      }  
      w <- newname
  }
  x <- select_(x, .dots = c(vars, byvars, w))
  # bug for do in data.table
  out <- do_(x, ~describe(., d = d, wname = w, byvars = byvars))
  out <- arrange_(out, .dots = c(byvars, "Variable"))
  out <- select_(out, .dots = c(byvars, "Variable", setdiff(names(out), c("Variable", byvars))))
  print_pretty_summary(out, byvars)
  invisible(out)
}



describe <- function(M, d = FALSE, wname = character(0),  byvars = character(0)){
  if (length(byvars)){
    M <- select_(M, ~-one_of(byvars))
  }
  if (length(wname)){
    w <- M[[wname]]
    M <- select_(M, ~-one_of(wname))
  }
  else{
    w <- NULL
  }
  names <- names(M)
  # Now starts the code 
  if (d==FALSE) {
    if (!is.null(w)){
      sum <- lapply(M ,function(x){
        take <- !is.na(x) & !is.na(w) & w > 0
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
    sum <- do.call(cbind, sum)
    sum <- as.data.frame(t(sum))
    sum <- bind_cols(data_frame(names), sum)
    sum <- setNames(sum, c("Variable", "Obs","Missing","Mean","StdDev","Min", "Max"))
  } else {
    N <- nrow(M)
    f=function(x){
      if (!is.null(w)){
        take <- !is.na(x) & !is.na(w) & w > 0
        x_omit <- x[take]
        w_omit <- w[take]
        m <- matrixStats::weightedMean(x_omit, w = w_omit)
        sum_higher <- matrixStats::colWeightedMeans(cbind((x_omit-m)^2,(x_omit-m)^3,(x_omit-m)^4), w = w_omit)
        sum_higher[1] <- sqrt(sum_higher[1])
        sum_higher[2] <- sum_higher[2]/sum_higher[1]^3
        sum_higher[3] <- sum_higher[3]/sum_higher[1]^4
        sum_quantile <- pctile(x_omit, c(0, 0.01, 0.05, 0.1, 0.25, 0.50, 0.75, 0.9, 0.95, 0.99, 1), w = w_omit)
      } else{
        x_omit <- na.omit(x)
        m <- mean(x_omit)
        sum_higher <- colMeans(cbind((x_omit-m)^2,(x_omit-m)^3,(x_omit-m)^4))
        sum_higher[1] <- sqrt(sum_higher[1])
        sum_higher[2] <- sum_higher[2]/sum_higher[1]^3
        sum_higher[3] <- sum_higher[3]/sum_higher[1]^4
        sum_quantile= pctile(x_omit, c(0, 0.01, 0.05, 0.1, 0.25, 0.50, 0.75, 0.9, 0.95, 0.99, 1))
      }
      n_NA <- length(x) - length(x_omit)
      sum <- c(N-n_NA, n_NA, m, sum_higher, sum_quantile)
    }
    sum <- mclapply(M, f)
    sum <- do.call(cbind, sum)
    sum <- as.data.frame(t(sum))
    sum <- bind_cols(data_frame(names), sum)
    sum <- setNames(sum,  c("Variable", "Obs","Missing","Mean","StdDev","Skewness","Kurtosis","Min","p1","p5","p10","p25","p50","p75","p90","p95","p99","Max"))
  }
  sum
}



print_pretty_summary <- function(x, byvars){
  if ("Skewness" %in% names(x)){
    x1 <- select_(x, ~one_of(c(byvars, "Variable", "Obs", "Missing", "Mean", "StdDev", "Skewness", "Kurtosis")))
    x2 <- select_(x, ~one_of(c(byvars, "Variable", "Min", "p1", "p5", "p10", "p25", "p50")))
    x3 <- select_(x, ~one_of(c(byvars, "Variable", "p50", "p75", "p90", "p95", "p99", "Max")))
    statascii(x1, n_groups = length(byvars) + 1)
    cat("\n")
    statascii(x2, n_groups = length(byvars) + 1)
    cat("\n")
    statascii(x3, n_groups = length(byvars) + 1)
  } else{
    statascii(x, n_groups = length(byvars) + 1)
  }
}