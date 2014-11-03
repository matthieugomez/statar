#' Fuzzy join two data.tables together 
#'
#' \code{fuzzy_join} uses record linkage methods to match observations between two datasets where no perfect key fields exist.  For each row in x, \code{fuzzy_join} finds the closest row(s) in y. The distance is a weighted average of the string distances defined in \code{method} over multiple columns.
#'
#' @param x The master data.table
#' @param y The using data.table
#' @param exact Character vector specifying variables on which to match exactly. 
#' @param exact.or.NA Character vector specifying variables that should not differ if both are non missing.
#' @param fuzzy Character vector specifying columns on which to match in a fuzzy way
#' @param check. With m~m, multiple to multiple matches. With, 1~1, the match is one to one and is solved through linear optimization among observations that are not exactly matched (experimental)
#' @param gen Name of new variable with the distance between matched observations. Default to "distance".
#' @param suffixes A character vector of length 2 specifying  suffix of overlapping columns. Defaut to ".x" and ".y".
#' @param which With \code{which = TRUE}, returns a three columns data.tables where he first column corresponds to \code{x}'s row number, the second column corresponds to \code{y}'s row number and the third column corresponds to the score of the match. Default is \code{FALSE}, which returns a join with the rows in y.
#' @param w Numeric vector of the same length as \code{fuzzy} specifying the weights to use when summing across different column of \code{fuzzy}. Default to \code{rep(1, length(fuzzy))}.
#' @param na.score Numeric that specifies the distance between NA and another string. Default to 1/3
#' @param mc.cores Number of cores to use
#' @param method See the \code{\link[stringdist]{stringdist}} documentation. Default to \code{"jw"}
#' @param p See  the \code{\link[stringdist]{stringdist}} documentation. Default to \code{0.1}
#' @param ... Other arguments to pass to \code{stringdist}. See the \code{\link[stringdist]{stringdist}} documentatio
#' @examples
#' library(data.table)
#' x <- data.table(a = c("france", "franc"), b = c("arras", "dijon"))
#' y <- data.table(a = c("franc", "france"), b = c("arvars", "dijjon"))
#' fuzzy_join(x, y, fuzzy = c("a", "b"))
#' fuzzy_join(x, y, fuzzy = c("a","b"), check = m~1)
#' fuzzy_join(x, y, fuzzy = c("a", "b"), w = c(0.9, 0.1))
#' fuzzy_join(x,y, fuzzy = c("a", "b"), w = c(0, 0.9))
#' x <- data.table(a = c(1, 1), b = c("arras", "dijon"))
#' y <- data.table(a = c(1, 1), b = c("arvars", "dijjon"))
#' fuzzy_join(x, y, exact = "a", fuzzy = "b")
#' x <- data.table(a = c(1, 2), b = c("arras", "dijon"))
#' y <- data.table(a = c(1, 1), b = c("arvars", "dijjon"))
#' fuzzy_join(x, y, exact = "a", fuzzy = "b")
#' fuzzy_join(x, y, exact = "a", fuzzy = "b", check = m~1)
#'
#' @details Typically, \code{x} is a dataset with dirty names, while \code{y} is the dataset with true names. When \code{exact} or \code{exact.or.NA} is specified, rows without matches are returned with distance NA. The algorithm first creates a cross product of both tables (or a left_join if exact is not NULL), which requires memory. You may want to process x by chunks.
#' @export
fuzzy_join <- function(x, y, exact = NULL, exact.or.NA = NULL, fuzzy = NULL, check = m~m, gen = "distance", suffixes = c(".x",".y"), which = FALSE, w = rep(1, length(fuzzy)), na.score = 1/3, mc.cores = getOption("mc.cores", 2L), method = "jw", p = 0.1, ...){
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
  temp.v <- tempname(c(names(x), names(y), index.x, index.y))

  w <- w/sum(w)

  # remove duplicates with respect to key columns in x and y
  ans.x <- keep_(x, c(exact, exact.or.NA, fuzzy))
  ans.y <- keep_(y, c(exact, exact.or.NA, fuzzy))
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
  exact.matched <- suppressMessages(join(ans.x, ans.y, check = check, kind = "inner", on = c(exact, exact.or.NA, fuzzy)))
  exact.matched <- keep_(exact.matched, c(index.x, index.y))
  exact.matched[, (gen) := 0]
  length <- n_distinct(exact.matched[[index.x]])
  message(paste(length,"rows of x are perfectly matched"))
  if (check[[3]] == 1){
  	ans.y2 <- suppressMessages(join(ans.y, ans.x, kind = "anti", on = c(exact, exact.or.NA, fuzzy)))
  	ans.x <- suppressMessages(join(ans.x, ans.y, kind = "anti", on = c(exact, exact.or.NA, fuzzy)))
  	ans.y <- ans.y2
  } else{
  	ans.x <- suppressMessages(join(ans.x, ans.y, kind = "anti", on = c(exact, exact.or.NA, fuzzy)))
  }

  # join by variables in exact
  if (length(exact)){
	  ans <- suppressMessages(join(ans.x, ans.y, on = exact, kind = "left"))
	  ans[, c(exact) := NULL]
	} else{
		ans <- suppressMessages(join(ans.x, ans.y, kind = "cross"))
	}

	# remove rows with exact.or.NA not satisfied
	if (length(exact.or.NA)){
  	condition.exact.or.NA <-  sapply(seq_along(exact.or.NA),
  	  function(i){
  	     pastem(exact.or.NA, ".x == ",exact.or.NA, ".y | is.na(", exact.or.NA, ") | is.na(",exact.or.NA,")", sep = "")
  	    })
  	condition.exact.or.NA <- paste0("(",paste(condition.exact.or.NA, collapse = ")&("),")")
  	ans.y <- keep_if_(ans, condition.exact.or.NA)
  	ans[, c(paste0(exact.or.NA, ".x")) := NULL]
  	ans[, c(paste0(exact.or.NA, ".y")) := NULL]
	}

	# compute
	ans[, c(temp.v) := 0]
	for (i in seq_along(fuzzy)){
	  ans[, c(temp.v) := get(temp.v) + w[i]*stringdist2(get(paste0(fuzzy[i], ".x")), get(paste0(fuzzy[i], ".y")), method = method, p = p, na.score = na.score, ...)]
	  ans[, c(paste0(fuzzy[i], ".x")) := NULL]
	  ans[, c(paste0(fuzzy[i], ".y")) := NULL]
	}
	if (check[[3]] == 1){
    # keep only best score among pairs
    aux <- ans[, .I[which.min(get(temp.v))], by = c(index.x, index.y)]
    aux <- aux[[length(aux)]]
    ans <- ans[aux]
    # then compute optimization
		nx <- n_distinct(ans[[index.x]])
    ny <- n_distinct(ans[[index.y]])
		n <- nrow(ans)
		C1 <- array(0, dim = c(nx, n))
		C1[cbind(ans[[index.x]], seq_len(n))] <- 1
		C2 <- array(0, dim = c(ny, n))
		C2[cbind(ans[[index.y]], seq_len(n))] <- 1
		C <- rbind(C1, C2)
		B <- rep(1, nrow(C))
		res <- lp("max", objective.in = 1 - ans[[temp.v]], const.mat = C, const.dir = rep("<=", nx + 
		    ny), const.rhs = B, all.bin = TRUE)
		aux <- which(res$solution > 0)
	} else{
		aux <- ans[, .I[which.min(get(temp.v))], by = index.x]
		aux <- aux[[length(aux)]]
	}
	result <- ans[aux]

  # append exact and fuzzy matching
  setnames(result, c(index.x, index.y, gen))
  out <- rbind(exact.matched, result, use.names = TRUE)

  # add back duplicated
  out <- suppressMessages(join(out, merge.x, on = index.x, kind = "left"))
  out <- suppressMessages(join(out, merge.y, on = index.y, kind = "left"))
  setkeep_(out, c("x","y", gen))

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


score_row <- function(l, index.y, ans.y, exact = NULL, exact.or.NA = NULL, fuzzy = NULL, w = rep(1, length(fuzzy)), ...){
  # binary search
  if (length(exact)){
    ans.y <- ans.y[l[exact], nomatch = 0]
  }
  if (length(exact.or.NA)){
    condition.exact.or.NA <-  sapply(seq_along(exact.or.NA),
      function(i){
        if (!is.na(l[[exact.or.NA[i]]])){
          pastem(exact.or.NA[i], "==", l[[exact.or.NA[i]]], "| is.na(",l[[exact.or.NA[i]]],"))")
        }  
      })
    condition.exact.or.NA <- paste0("(",paste(condition.exact.or.NA, collapse = ")&("),")")
    ans.y <- keep_if_(ans.y, condition.exact.or.NA)
  } 
  if (nrow(ans.y)==0){
  	return(c(NA, NA))
  } else{
    tempv <- rep(0, nrow(ans.y))
    for (i in seq_along(fuzzy)){
  	  tempv <- tempv + w[i]*stringdist2(l[[fuzzy[i]]], ans.y[[fuzzy[i]]], ...)
    }
    index <- which.min(tempv)
    return(c(ans.y[[index.y]][index], tempv[index]))
	}
}

  

stringdist2 <- function(x, y, na.score,  ...){
  out <- stringdist(x,y, ...)
  out[is.na(x)] <- na.score
  out[is.na(y)] <- na.score
  out
}




