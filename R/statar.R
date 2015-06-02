#' A package for applied research
#'
#' @docType package
#' @name statar
#' @import data.table 
#' @import ggplot2
#' @importFrom dplyr funs_
#' @importFrom dplyr with_order
#' @importFrom dplyr mutate_each_
#' @importFrom dplyr slice
#' @importFrom dplyr select_vars_
#' @importFrom dplyr select_
#' @importFrom dplyr tbl_vars
#' @importFrom dplyr filter_
#' @importFrom dplyr group_by
#' @importFrom dplyr n_distinct
#' @importFrom dplyr sample_n
#' @importFrom dplyr arrange_
#' @importFrom dplyr group_by_
#' @importFrom dplyr summarize
#' @importFrom dplyr n
#' @importFrom dplyr funs
#' @importFrom dplyr summarize_
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr data_frame
#' @importFrom grid grid.layout
#' @importFrom grid grid.newpage
#' @importFrom grid pushViewport
#' @importFrom grid viewport
#' @importFrom lfe demeanlist
#' @importFrom lfe felm
#' @importFrom lazyeval as.lazy
#' @importFrom lazyeval lazy_dots
#' @importFrom lazyeval lazy_eval
#' @importFrom lazyeval all_dots
#' @importFrom lazyeval common_env
#' @importFrom lazyeval interp
#' @importFrom lazyeval lazy
#' @importFrom lubridate period
#' @importFrom lubridate second
#' @importFrom lubridate weeks
#' @importFrom lubridate years
#' @importFrom matrixStats weightedMean
#' @importFrom matrixStats colWeightedMeans
#' @importFrom matrixStats colRanges
#' @importFrom parallel mclapply
#' @importFrom stargazer stargazer
#' @importFrom stats lag
#' @importFrom stringdist stringdist
#' @importFrom stringdist stringdistmatrix
#' @importFrom stringr str_replace
#' @importFrom stringr str_match
#' @importFrom stringr str_detect
#' @importFrom stringr str_split
#' @importFrom stringr fixed
#' @importFrom tidyr gather_
#' @importFrom tidyr spread
#' @importFrom tidyr spread_
NULL



#' @useDynLib statar
#' @importFrom Rcpp sourceCpp
NULL