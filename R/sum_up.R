#' Gives summary statistics (corresponds to Stata command summarize)
#' 
#' @param x a data.frame
#' @param ... Variables to include. Defaults to all non-grouping variables. See the \link[dplyr]{select} documentation.
#' @param w Weights. Default to NULL. 
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
#' sum_up(df, v2, wt = v1)
#' df %>% group_by(v1) %>% sum_up(starts_with("v"))
#' @return a data.frame 

sum_up <- function(df, ...,  d = FALSE, wt = NULL) {
  wt = enquo(wt)
  if (rlang::is_null(rlang::f_rhs(wt))) {
    wtvar <- character(0)
  }
  else{
    wtvar <- names(dplyr::select_vars(names(df), !!wt))
  }
  byvars <- dplyr::group_vars(df)
  vars <- setdiff(names(dplyr::select_vars(names(df), ...)), c(wtvar, byvars))
  if (length(vars) == 0) {
     vars <- setdiff(names(df), c(byvars, wtvar))
  }
  nums <- sapply(df, is.numeric)
  nums_name <- names(nums[nums == TRUE])
  vars <- intersect(vars, nums_name)
  if (!length(vars)) stop("Please select at least one non-numeric variable", call. = FALSE)
  df <- dplyr::select_at(df, c(vars, byvars, wtvar))
  # bug for do in data.table
  df <- dplyr::do(df, describe(., d = d, wtvar = wtvar, byvars = byvars))
  out <- dplyr::arrange_at(df, c(byvars, "Variable"))
  # reorder
  if (d) {
    out1 <- dplyr::select_at(out, c(byvars, "Variable", "Obs", "Missing", "Mean", "StdDev", "Skewness", "Kurtosis"))
    out2 <- dplyr::select_at(out, c(byvars, "Variable", "Min", "p1", "p5", "p10", "p25", "p50"))
    out3 <- dplyr::select_at(out, c(byvars, "Variable", "p50", "p75", "p90", "p95", "p99", "Max"))
    statascii(out1, n_groups = length(byvars) + 1)
    cat("\n")
    statascii(out2, n_groups = length(byvars) + 1)
    cat("\n")
    statascii(out3, n_groups = length(byvars) + 1)
  } 
  else{
    #reorder
    out <- dplyr::select_at(out, c(byvars, "Variable", setdiff(names(out), c(byvars, "Variable"))))
    statascii(out, n_groups = length(byvars) + 1)
  }
  invisible(out)
}



describe <- function(df, d = FALSE, wtvar = character(0),  byvars = character(0)){
  if (length(byvars)){
    df <- dplyr::select_at(df, setdiff(names(df), byvars))
  }
  if (length(wtvar)){
    w <- df[[wtvar]]
    df <- dplyr::select_at(df, setdiff(names(df), wtvar))
  }
  else{
    w <- NULL
  }
  names <- names(df)
  # Now starts the code 
  if (d==FALSE) {
    if (!is.null(w)){
      sum <- lapply(df ,function(x){
        take <- !is.na(x) & !is.na(w) & w > 0
        x_omit <- x[take]
        w_omit <- w[take]
        m <- matrixStats::weightedMean(x_omit, w = w_omit)
        c(length(x_omit), length(x)-length(x_omit), m, sqrt(matrixStats::weightedMean((x_omit-m)^2, w = w_omit)), matrixStats::colRanges(x_omit, dim = c(length(x_omit), 1)))
      })
    }else{
      sum <- lapply(df ,function(x){
        x_omit <- na.omit(x)
      c(length(x_omit), length(x) - length(x_omit), mean(x_omit), sd(x_omit), matrixStats::colRanges(x_omit, dim = c(length(x_omit), 1)))
      })
    }
    sum <- do.call(cbind, sum)
    sum <- as.data.frame(t(sum))
    sum <- dplyr::bind_cols(data_frame(names), sum)
    sum <- setNames(sum, c("Variable", "Obs","Missing","Mean","StdDev","Min", "Max"))
  } else {
    N <- nrow(df)
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
    sum <- mclapply(df, f)
    sum <- do.call(cbind, sum)
    sum <- as.data.frame(t(sum))
    sum <- dplyr::bind_cols(data_frame(names), sum)
    sum <- setNames(sum,  c("Variable", "Obs","Missing","Mean","StdDev","Skewness","Kurtosis","Min","p1","p5","p10","p25","p50","p75","p90","p95","p99","Max"))
  }
  sum
}
