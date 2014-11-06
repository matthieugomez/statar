#' select variables in a data.frame
#'
#' @param x a data.frame
#' @param ...  See the \link[dplyr]{select} documentation.
#' @param .dots Needed to work around non-standard evaluation
#' @return a character vector. All the syntax available in \code{select_vars} are allowed. Characters are understood as wildcards.
#' @examples
#' df <- data.frame(id = 3, id2 = 4)
#' vars(df, id)
#' vars(df, w("id*"))
#' @export
vars <- function(x, ...) {
  args <- lazyeval::lazy_dots(...)
  vars_(x, args)
}

#' @export
#' @rdname vars
vars_not <- function(x, ...){
	setdiff(names(x), vars(x, ...))
}

#' @export
#' @rdname vars
vars_not_ <- function(x, .dots){
	setdiff(names(x), vars_(x, .dots))
}


#' @export
#' @rdname vars
vars_ <- function(x, .dots) {
  include <- character(0)
  exclude <- character(0)
  varsname <- names(x)

  args <- lazyeval::as.lazy_dots(.dots)

  # No non-standard evaluation - but all names mapped to their position.
  # Keep integer semantics: include = +, exclude = -
  names_list <- setNames(as.list(seq_along(varsname)), varsname)

  select_funs <- list(
    starts_with = function(...) starts_with(varsname, ...),
    ends_with = function(...) ends_with(varsname, ...),
    contains = function(...) contains(varsname, ...),
    matches = function(...) matches(varsname, ...),
    num_range = function(...) num_range(varsname, ...),
    one_of = function(...) one_of(varsname, ...),
    everything = function(...) everything(varsname, ...),
    w = function(...) w(varsname, ...)

  )

  ind_list <- lazyeval::lazy_eval(args, c(names_list, select_funs))
  names(ind_list) <- names2(args)

  ind <- unlist(ind_list)
  incl <- ind[ind > 0]

  # If only negative values, implicitly assumes all variables to be removed.
  if (sum(ind > 0) == 0 && sum(ind < 0) > 0) {
    incl <- seq_along(varsname)
  }
  # Remove duplicates (unique loses names)
  incl <- incl[!duplicated(incl)]

  # Remove variables to be excluded (setdiff loses names)
  excl <- abs(ind[ind < 0])
  incl <- incl[match(incl, excl, 0L) == 0L]

  bad_idx <- incl < 0 | incl > length(varsname)
  if (any(bad_idx)) {
    stop("Bad indices: ", paste0(which(bad_idx), collapse = ", "),
      call. = FALSE)
  }
  # Include/exclude specified variables
  sel <- setNames(varsname[incl], names(incl))
  sel <- c(setdiff2(include, sel), sel)
  sel <- setdiff2(sel, exclude)


  # Ensure all output varsname named
  if (length(sel) == 0) {
    names(sel) <- sel
  } else {
    unnamed <- names2(sel) == ""
    names(sel)[unnamed] <- sel[unnamed]
  }

  sel
}




starts_with <- function(vars, match, ignore.case = TRUE) {
  stopifnot(is.character(match), !is.na(match), nchar(match) > 0)

  if (ignore.case) match <- tolower(match)
  n <- nchar(match)

  if (ignore.case) vars <- tolower(vars)
  which(substr(vars, 1, n) == match)
}

ends_with <- function(vars, match, ignore.case = TRUE) {
  stopifnot(is.character(match), !is.na(match), nchar(match) > 0)

  if (ignore.case) match <- tolower(match)
  n <- nchar(match)

  if (ignore.case) vars <- tolower(vars)
  length <- nchar(vars)

  which(substr(vars, pmax(1, length - n + 1), length) == match)
}

contains <- function(vars, match, ignore.case = TRUE) {
  stopifnot(is.character(match), nchar(match) > 0)

  if (ignore.case) {
    vars <- tolower(vars)
    match <- tolower(match)
  }
  grep(match, vars, fixed = TRUE)
}

matches <- function(vars, match, ignore.case = TRUE) {
  stopifnot(is.character(match), nchar(match) > 0)

  grep(match, vars, ignore.case = ignore.case)
}

num_range <- function(vars, prefix, range, width = NULL) {
  if (!is.null(width)) {
    range <- sprintf(paste0("%0", width, "d"), range)
  }
  match(paste0(prefix, range), vars)
}

one_of <- function(vars, ...) {
  keep <- c(...)
  stopifnot(is.character(keep))
  match(keep, vars)
}

everything <- function(vars) {
  seq_along(vars)
}

w <- function(vars, match, ignore.case = TRUE){
  stopifnot(is.character(match), nchar(match) > 0)
  grep(glob2rx(match), vars, ignore.case = ignore.case)
}


r <- function(vars, match, ignore.case = TRUE){
  stopifnot(is.character(match), nchar(match) > 0)
  grep(match, vars, ignore.case = ignore.case)
}


names2 <- function(x) {
  names(x) %||% rep("", length(x))
}

"%||%" <- function(x, y) if(is.null(x)) y else x


setdiff2 <- function(x, y) {
  x[match(x, y, 0L) == 0L]
}
