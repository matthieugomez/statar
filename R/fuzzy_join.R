#' Join two data.tables together 
#' 
#' @param x The master data.table
#' @param y The using data.table
#' @param on Exact: exact characters
#' @param exact.or.NA Exact or NA
#' @param fuzzy Fuzzy columns on which to match
#' @param w Vector of weights
#' @param methods see stringdist
#' @param ... argument to pass to stringdist
#' @export
fuzzy_join =  function(x, y, exact = NULL, exact.or.NA =NULL, fuzzy = NULL, w = rep(1, length(fuzzy)), gen = "distance", which = FALSE, method = "jw", p = 0.1, na.score = 1/3, ...){
  if (gen %in% union(names(x), names(y))) stop(gen, "already exists")
  index.x <- tempname(c(names(x), names(y)))
  index.y <- tempname(c(names(x), names(y), index.x))
  w <- w/sum(w)
  ans.x <- keep_(x, c(exact, exact.or.NA, fuzzy))
  ans.y <- keep_(y, c(exact, exact.or.NA, fuzzy))
  ans.x[, c(index.x) := .I]
  ans.y[, c(index.y) := .I]
  # exact matching
  if (length(c(exact, exact.or.NA, fuzzy))){
    exact.matched <- join(ans.x, ans.y, kind = "inner", on = c(exact, exact.or.NA, fuzzy))
    exact.matched <- keep_(exact.matched, c(index.x, index.y))
    exact.matched[, (gen) := 0]
    setnames(exact.matched, c("x","y", gen))
    ans.x <- join(ans.x, ans.y, kind = "anti", on = c(exact, exact.or.NA, fuzzy))
  }
  # fuzzy matching
  result <- sapply(seq_len(nrow(ans.x)), function(i){
    c(ans.x[[index.x]][i], f(l = ans.x[i], index.y = index.y, ans.y = ans.y, exact = exact, exact.or.NA = exact.or.NA, fuzzy = fuzzy, w = w, method = method, p = p, na.score = na.score, ...))
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
    setnames(xx, common_names, paste0(common_names, ".x"))
    setnames(yy, common_names, paste0(common_names, ".y"))
    out <- cbind(xx, yy)
  }
  out
}


f <- function(l, index.y, ans.y, exact = NULL, exact.or.NA = NULL, fuzzy = NULL, w = rep(1, length(fuzzy)), ...){
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
    ans.y <- keep_if(ans.y, condition)
  }
  tempv <- sapply(seq_along(fuzzy), function(i){
    w[i]*stringdist2(l[[fuzzy[i]]], ans.y[[fuzzy[i]]], ...)
  })
  agg <- rowSums(tempv)
  index <- which.min(agg)
  c(ans.y[[index.y]][index], agg[index])
}

  

stringdist2 <- function(x, y, na.score,  ...){
  out <- stringdist(x,y, ...)
  out[is.na(x)] <- na.score
  out[is.na(y)] <- na.score
  out
}




