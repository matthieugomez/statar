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
#' @param p See  the \code{\link[stringdist]{stringdist}} documentation. Default to 0.1
#' @param ... Other arguments to pass to \code{stringdist}. See the \code{\link[stringdist]{stringdist}} documentation.
#' @examples
#' x <- data.table(x = c("france", "franc"), y = c("arras", "dijon"))
#' y <- data.table(x = c("franc", "france"), y = c("arvars", "dijjon"))
#' fuzzy_join(x, y, fuzzy = c("x", "y"))
#' fuzzy_join(x, y, fuzzy = c("x", "y"), w = c(0.9, 0.1))
#' fuzzy_join(x,y, fuzzy = c("x", "y"), w = c(0, 0.9))
#' x <- data.table(x = c(1, 1), y = c("arras", "dijon"))
#' y <- data.table(x = c(1, 1), y = c("arvars", "dijjon"))
#' fuzzy_join(x, y, exact = "x", fuzzy = "y")
#' x <- data.table(x = c(1, 2), y = c("arras", "dijon"))
#' y <- data.table(x = c(1, 1), y = c("arvars", "dijjon"))
#' fuzzy_join(x,y, exact = "x", fuzzy = "y")
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
  index.x <- tempname(c(names(x), names(y)))
  index.y <- tempname(c(names(x), names(y), index.x))
  w <- w/sum(w)
  ans.x <- keep_(x, c(exact, exact.or.NA, fuzzy))
  ans.y <- keep_(y, c(exact, exact.or.NA, fuzzy))
  ans.x[, c(index.x) := .I]
  ans.y[, c(index.y) := .I]
  # exact matching
  if (length(c(exact, exact.or.NA, fuzzy))){
    exact.matched <- suppressMessages(join(ans.x, ans.y, kind = "inner", on = c(exact, exact.or.NA, fuzzy)))
    exact.matched <- keep_(exact.matched, c(index.x, index.y))
    exact.matched[, (gen) := 0]
    setnames(exact.matched, c("x","y", gen))
    ans.x <- suppressMessages(join(ans.x, ans.y, kind = "anti", on = c(exact, exact.or.NA, fuzzy)))
  }
  # fuzzy matching
  result <- sapply(seq_len(nrow(ans.x)), function(i){
    c(ans.x[[index.x]][i], score_row(l = ans.x[i], index.y = index.y, ans.y = ans.y, exact = exact, exact.or.NA = exact.or.NA, fuzzy = fuzzy, w = w, method = method, p = p, na.score = na.score, ...))
    })
  result <- t(result)
  result <- as.data.table(result)
  setnames(result, c("x","y", gen))
  out <- rbind(exact.matched, result, use.names = TRUE)
  if (!which){
    xx <- x[out[["x"]]]
    yy <- y[out[["y"]]]
    yy <- yy[, (gen) := out[[gen]]]
    common_names = intersect(names(x),names(y))
    setnames(xx, common_names, paste0(common_names, suffixes[1]))
    setnames(yy, common_names, paste0(common_names, suffixes[2]))
    out <- cbind(xx, yy)
  }
  out
}


score_row <- function(l, index.y, ans.y, exact = NULL, exact.or.NA = NULL, fuzzy = NULL, w = rep(1, length(fuzzy)), ...){
  if (length(exact)){
    condition.exact <- sapply(seq_along(exact), 
      function(i){
      pastem(exact[i],"==", l[[exact[i]]])
    })
    condition.exact <- paste0("(",paste(condition.exact, collapse = ")&("),")")
  } else {condition.exact <- NULL}
  if (length(exact.or.NA)){
    condition.exact.or.NA <-  sapply(seq_along(exact.or.NA),
      function(i){
        if (!is.na(l[[exact.or.NA[i]]])){
          pastem(exact.or.NA[i], "==", l[[exact.or.NA[i]]], "| is.na(",l[[exact.or.NA[i]]],"))")
        }  
      })
    condition.exact.or.NA <- paste0("(",paste(condition.exact.or.NA, collapse = ")&("),")")
  } else {condition.exact.or.NA <- NULL}
  condition <- paste(condition.exact, condition.exact.or.NA)
  if (length(condition)){
    ans.y <- keep_if_(ans.y, condition)
  }
  if (nrow(ans.y)==0){
  	return(c(NA, NA))
  } else{
	  tempv <- sapply(seq_along(fuzzy), function(i){
	    w[i]*stringdist2(l[[fuzzy[i]]], ans.y[[fuzzy[i]]], ...)
	  })
	  agg <- rowSums(tempv)
	  index <- which.min(agg)
	  return(c(ans.y[[index.y]][index], agg[index]))
	}
}

  

stringdist2 <- function(x, y, na.score,  ...){
  out <- stringdist(x,y, ...)
  out[is.na(x)] <- na.score
  out[is.na(y)] <- na.score
  out
}




