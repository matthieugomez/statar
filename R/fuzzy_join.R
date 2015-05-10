#' Fuzzy join two data.tables together 
#'
#' \code{fuzzy_join} uses record linkage methods to match observations between two datasets where no perfect key fields exist.  For each row in x, \code{fuzzy_join} finds the closest row(s) in y. The distance is a weighted average of the string distances defined in \code{method} over multiple columns.
#'
#' @param x The master data.table
#' @param y The using data.table
#' @param exact Character vector specifying variables on which to match exactly. 
#' @param exact.or.NA Character vector specifying variables that should not differ if both are non missing.
#' @param fuzzy Character vector specifying columns on which to match in a fuzzy way
#' @param gen Name of new variable with the distance between matched observations. Default to "distance".
#' @param suffixes A character vector of length 2 specifying  suffix of overlapping columns. Defaut to ".x" and ".y".
#' @param which With \code{which = TRUE}, returns a three columns data.tables where he first column corresponds to \code{x}'s row number, the second column corresponds to \code{y}'s row number and the third column corresponds to the score of the match. Default is \code{FALSE}, which returns a join with the rows in y.
#' @param w Numeric vector of the same length as \code{fuzzy} specifying the weights to use when summing across different column of \code{fuzzy}. Default to \code{rep(1, length(fuzzy))}.
#' @param na.score Numeric that specifies the distance between NA and another string. Default to 1/3
#' @param method See the \code{\link[stringdist]{stringdist}} documentation. Default to \code{"jw"}
#' @param p See  the \code{\link[stringdist]{stringdist}} documentation. Default to \code{0.1}
#' @param ... Other arguments to pass to \code{stringdist}. See the \code{\link[stringdist]{stringdist}} documentation.
#' @examples
#' library(data.table)
#' x <- data.table(a = c("france", "franc"), b = c("arras", "dijon"))
#' y <- data.table(a = c("franc", "france"), b = c("arvars", "dijjon"))
#' fuzzy_join(x, y, fuzzy = c("a", "b"))
#' fuzzy_join(x, y, fuzzy = c("a", "b"), w = c(0.9, 0.1))
#' fuzzy_join(x,y, fuzzy = c("a", "b"), w = c(0, 0.9))
#' x <- data.table(a = c(1, 1), b = c("arras", "dijon"))
#' y <- data.table(a = c(1, 1), b = c("arvars", "dijjon"))
#' fuzzy_join(x, y, exact = "a", fuzzy = "b")
#' x <- data.table(a = c(1, 2), b = c("arras", "dijon"))
#' y <- data.table(a = c(1, 1), b = c("arvars", "dijjon"))
#' fuzzy_join(x, y, exact = "a", fuzzy = "b")
#' x <- data.table(a = c(1, NA), b = c("arras", "dijon"))
#' y <- data.table(a = c(1, 1), b = c("arvars", "dijjon"))
#' fuzzy_join(x, y, exact.or.NA = "a", fuzzy = "b")
#' @details Typically, \code{x} is a dataset with dirty names, while \code{y} is the dataset with true names. When \code{exact} or \code{exact.or.NA} is specified, rows without matches are returned with distance NA.
#' @export
fuzzy_join <- function(x, y, exact = NULL, exact.or.NA = NULL, fuzzy = NULL, gen = "distance", suffixes = c(".x",".y"), which = FALSE, w = rep(1, length(fuzzy)), na.score = 1/3, method = "jw", p = 0.1, ...){
  if (gen %in% union(names(x), names(y))) stop(gen, "already exists")
	if (!is.data.table(x)){
	  stop(paste0("x is not a data.table. Convert it first using setDT(x)"))
	}
	if (!is.data.table(y)){
	  stop(paste0("y is not a data.table. Convert it first using setDT(y)"))
	}
  if (!(length(w)==length(fuzzy))){
    stop("fuzzy and w must have the same length)")
  }
  index.x <- tempname(c(names(x), names(y)))
  index.y <- tempname(c(names(x), names(y), index.x))
  w <- w/sum(w)

  # remove duplicates with respect to key columns in x and y
  ans.x <- x[, c(exact, exact.or.NA, fuzzy), with = FALSE]
  ans.y <- y[, c(exact, exact.or.NA, fuzzy), with = FALSE]
  ## create unique identifiers .GRP
  ans.x[, c(index.x) := .GRP, by = c(exact, exact.or.NA, fuzzy)]
  ans.y[, c(index.y) := .GRP, by = c(exact, exact.or.NA, fuzzy)]
  ## keep link .GRP and row number
  merge.x <- ans.x[, list(get(index.x), .I)]
  merge.y <- ans.y[, list(get(index.y), .I)]
  setnames(merge.x, c(index.x, "x"))
  setnames(merge.y, c(index.y, "y"))

  # group
  setkeyv(ans.x, c(exact, exact.or.NA, fuzzy))
  setkeyv(ans.y, c(exact, exact.or.NA, fuzzy))
  ans.x <- unique(ans.x)
  ans.y <- unique(ans.y)

  # exact matching
  exact.matched <- suppressMessages(join(ans.x, ans.y, kind = "inner", on = c(exact, exact.or.NA, fuzzy)))
  exact.matched <- exact.matched[, c(index.x, index.y), with = FALSE]
  exact.matched[, (gen) := 0]
  length <- n_distinct(exact.matched[[index.x]])
  message(paste(length,"rows of x are exactly matched on all variables"))
  ans.x <- suppressMessages(join(ans.x, ans.y, kind = "anti", on = c(exact, exact.or.NA, fuzzy)))

  # fuzzy matching
  setDF(ans.x)
  setkeyv(ans.y, c(exact, exact.or.NA, fuzzy))
  if (length(exact.or.NA)){
  	condition <- build_condition(exact.or.NA, ans.x)
  } else{
  	condition <-  NULL
  }

  setcolorder(ans.y, c(exact, setdiff(names(ans.y), exact)))
  result <- lapply(seq_len(nrow(ans.x)), function(i){
    c(ans.x[[index.x]][i], score_row(l = ans.x[i,], condition.exact.or.NA = condition[i], index.y = index.y, ans.y = ans.y, exact = exact, exact.or.NA = exact.or.NA, fuzzy = fuzzy, w = w, method = method, p = p, na.score = na.score, ...))
    })
  result <- simplify2array(result, higher = FALSE)
  result <- t(result)
  result <- as.data.table(result)

  # append exact and fuzzy matching
  setnames(result, c(index.x, index.y, gen))
  out <- rbind(exact.matched, result, use.names = TRUE)

  # add back duplicated
  out <- suppressMessages(join(out, merge.x, on = index.x, kind = "left"))
  out <- suppressMessages(join(out, merge.y, on = index.y, kind = "left"))
  out[, setdiff(names(out), c("x","y", gen)) := NULL]

  # if which = FALSE, output rows instead of row index
  if (!which){
    xx <- x[out[["x"]]]
    yy <- y[out[["y"]]]
    yy <- yy[, (gen) := out[[gen]]]
    common_names = intersect(names(x),names(y))
    setnames(xx, common_names, paste0(common_names, suffixes[1]))
    setnames(yy, common_names, paste0(common_names, suffixes[2]))
    out <- cbind(xx, yy)
    neworder <-  c(gen, setdiff(names(out), gen))
    setcolorder(out, neworder)
  }
  out[]
}


score_row <- function(l, condition.exact.or.NA, index.y, ans.y, exact = NULL, exact.or.NA = NULL, fuzzy = NULL, w = rep(1, length(fuzzy)), ...){
  # binary search
  if (length(exact)){
    ans.y <- ans.y[l[exact], nomatch = 0]
  }
  if (nrow(ans.y)==0){
    return(c(NA, NA))
  } 
  if (!is.null(condition.exact.or.NA) && condition.exact.or.NA!=""){
    expression <- parse(text = condition.exact.or.NA)
    ans.y <- eval(substitute(ans.y[v,], list(v = expression)))
  } 
  if (nrow(ans.y)==0){
    	return(c(NA, NA))
  } 
  tempv <- rep(0, nrow(ans.y))
  for (i in seq_along(fuzzy)){
	  tempv <- tempv + w[i]*stringdist2(l[[fuzzy[i]]], ans.y[[fuzzy[i]]], ...)
  }
  index <- which.min(tempv)
  return(c(ans.y[[index.y]][index], tempv[index]))
}


  

stringdist2 <- function(x, y, na.score,  ...){
  out <- stringdist(x,y, ...)
  out[is.na(x)] <- na.score
  out[is.na(y)] <- na.score
  out
}


build_condition <- function(exact.or.NA, ans.x){
  condition <- NULL
  for (i in seq_along(exact.or.NA)){
    condition <- paste0(condition, 
    	ifelse(
    		!is.na(ans.x[[exact.or.NA[i]]]),
    		paste0("&(", exact.or.NA[i], " == ", ans.x[[exact.or.NA[i]]], " | is.na(", exact.or.NA[i],"))"),
    		"")
    	)
  }
  str_replace(condition, fixed("&"), "")
}
