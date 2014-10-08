## imported from dplyr
dt_env <- function(dt, env) {
  env <- new.env(parent = env, size = 2L)
  env$dt <- dt
  env$vars <- deparse_all(groups(dt))
  env
}


deparse_all <- function(x) {
  deparse2 <- function(x) paste(deparse(x, width.cutoff = 500L), collapse = "")
  vapply(x, deparse2, FUN.VALUE = character(1))
}

tempname_list=function(prefix, l) {
    i <- 0L
    name <- prefix
    while (name %in% l) {
        i <- i + 1L
        name <- paste0(prefix, as.character(i))
    }
    name
}

