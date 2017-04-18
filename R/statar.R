#' A package for applied research
#'
#' @docType package
#' @name statar
#' @import ggplot2
#' @importFrom data.table is.data.table
#' @importFrom data.table key
#' @importFrom data.table setnames
#' @importFrom data.table setattr
#' @importFrom data.table setkeyv
#' @importFrom data.table setDF
#' @importFrom data.table setDT
#' @importFrom data.table as.data.table
#' @importFrom data.table dcast.data.table
#' @importFrom data.table :=
#' @importFrom dplyr "%>%"
#' @importFrom dplyr count
#' @importFrom dplyr desc
#' @importFrom dplyr filter
#' @importFrom dplyr distinct_
#' @importFrom dplyr everything
#' @importFrom dplyr ungroup
#' @importFrom dplyr group_indices_
#' @importFrom dplyr funs_
#' @importFrom dplyr with_order
#' @importFrom dplyr mutate_each_
#' @importFrom dplyr slice
#' @importFrom dplyr count_
#' @importFrom dplyr select_vars_
#' @importFrom dplyr select_
#' @importFrom dplyr tbl_vars
#' @importFrom dplyr filter_
#' @importFrom dplyr group_by
#' @importFrom dplyr n_distinct
#' @importFrom dplyr distinct
#' @importFrom dplyr sample_n
#' @importFrom dplyr arrange
#' @importFrom dplyr arrange_
#' @importFrom dplyr group_by_
#' @importFrom dplyr summarize
#' @importFrom dplyr n
#' @importFrom dplyr groups
#' @importFrom dplyr do
#' @importFrom dplyr do_
#' @importFrom dplyr funs
#' @importFrom dplyr summarize_
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_
#' @importFrom dplyr rename
#' @importFrom dplyr rename_
#' @importFrom dplyr select
#' @importFrom dplyr select_
#' @importFrom dplyr left_join
#' @importFrom dplyr right_join
#' @importFrom dplyr full_join
#' @importFrom dplyr inner_join
#' @importFrom dplyr semi_join
#' @importFrom dplyr anti_join
#' @importFrom dplyr data_frame
#' @importFrom dplyr bind_cols
#' @importFrom dplyr bind_rows
#' @importFrom dplyr as_data_frame
#' @importFrom dplyr one_of
#' @importFrom lazyeval as.lazy
#' @importFrom lazyeval lazy_dots
#' @importFrom lazyeval lazy_eval
#' @importFrom lazyeval all_dots
#' @importFrom lazyeval common_env
#' @importFrom lazyeval interp
#' @importFrom lazyeval lazy
#' @importFrom matrixStats weightedMean
#' @importFrom matrixStats colWeightedMeans
#' @importFrom matrixStats colRanges
#' @importFrom parallel mclapply
#' @importFrom stringr str_replace
#' @importFrom stringr str_match
#' @importFrom stringr str_detect
#' @importFrom stringr str_split
#' @importFrom stringr str_pad
#' @importFrom stringr str_sub
#' @importFrom stringr str_replace_na
#' @importFrom stringr fixed
#' @importFrom tidyr gather_
#' @importFrom tidyr spread
#' @importFrom tidyr spread_
#' @importFrom methods is
#' @importFrom stats as.formula
#' @importFrom stats complete.cases
#' @importFrom stats na.omit
#' @importFrom stats quantile
#' @importFrom stats sd
#' @importFrom stats setNames
#' @importFrom utils capture.output
#' @importFrom utils head
#' @importFrom utils tail
#' @importFrom utils type.convert
NULL

globalVariables(".SD")
globalVariables("Statbinmean")

