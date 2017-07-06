#' Returns cross tabulation
#' 
#' @param x a vector or a data.frame
#' @param ... Variable(s) to include. If length is two, a special cross tabulation table is printed although a long data.frame is always (invisibly) returned.
#' @param i Condition to apply function on certain rows only
#' @param w Frequency weights. Default to NULL. 
#' @param na.rm Remove missing values. Default to FALSE
#' @param .dots Used to work around non-standard evaluation.
#' @param sort Boolean. Default to TRUE
#' @examples
#' # setup
#' library(dplyr)
#' N <- 1e2 ; K = 10
#' df <- data_frame(
#'   id = sample(c(NA,1:5), N/K, TRUE),
#'   v1 =  sample(1:5, N/K, TRUE)                       
#' )
#' # one-way tabulation
#' df %>% tab(id)
#' df %>% tab(id, wt = v1)
#' # two-way tabulation
#' df %>% tab(id, v1)
#' df %>% filter(id >= 3) %>% tab(id)
#' @return a data.frame sorted by variables in ..., and with columns "Freq.", "Percent", and "Cum." for counts.
#' @export


#' @export
#' @rdname tab
tab <- function(x, ..., wt = NULL, na.rm = FALSE, sort = TRUE){
  x <- dplyr::count(x, ..., wt = !!enquo(wt))
  if (na.rm){
    x <- na.omit(x)
  }
  x <- dplyr::rename(x, Freq. = n)
  x <- dplyr::mutate(x, Percent = Freq./sum(Freq.)*100, Cum. = cumsum(Percent))
  if (sort){
    x <- dplyr::arrange(x, ...)
  }
  statascii(x, n_groups = ncol(x) - 3)
  invisible(x)
}