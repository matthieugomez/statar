#' Find best string combinations that identify an id
#'
#' @param id a vector of identifiers
#' @param v a vector of characters
#' @param n number of words for combinations. Default to \code{1}.
#' @return \code{tab_accross} returns a data.frame of four columns. The first is id, the second corresponds to unique combination of words in each element of \code{v} with length lower than \code{n} (sorted alphabetically),  the third is the count of these permutation within \code{id}, the fourth is the count of these permutation accross \code{i}. When the count accross group is 1 and the count within group is high, the element can be considered as an identifier of the group.
#' id <- c(1, 1, 2, 2)
#' names <- c("coca cola company", "coca cola incorporated", "apple incorporated", "apple corp")
#' count_combinations(id, v)
#' count_combinations(id, v, n = 2)
count_combinations <- function(id, names, n = 1){
  dt <- setDT(list(id = id, names = names))
  if (n>0){
    f <- function(x){
      g <- function(x){
          x <- unique(unlist(str_split(x, "\\s+")))
          out <- NULL
          for (i in seq_len(min(length(x), n))){
            out <- c(out, combn(x, i, function(x){paste(sort(x), collapse = " ")}))
          }
          out
      }
      out <- unlist(lapply(x, g))
      unname(out)
    } 
    dt <- dt[, list(names = f(names)), by = "id"]
  }
  dt <- dt[, list(.N) , by = c("id", "names")]
  setnames(dt, c("id", "names", "count_within"))
  dt[, count_across := .N, by = names]
  setorder(dt, id, count_across, -count_within)
  setDF(dt)
  dt
}



#' Find minimum distance of each word to other groups
#'
#' @param id a vector of identifiers
#' @param v a vector of characters
#' @param n number of words for combinations. Default to \code{0}.
#' @return \code{tab_accross} returns a data.frame of four columns. The first is id, the second corresponds to unique combination of words in each element of \code{v} with length lower than \code{n} (sorted alphabetically),  the third is the count of these permutation within \code{id}, the fourth is the count of these permutation accross \code{i}. When the count accross group is 1 and the count within group is high, the element can be considered as an identifier of the group.
#' id <- c(1, 1, 2, 2)
#' name <- c("coca cola company", "coca cola incorporated", "apple incorporated", "apple corp")
#' compute_score(id, name, n = 0)
#' compute_score(id, name, n = 1)
#' compute_score(id, name, n = 2)
compute_score <- function(id, name, n = 1, method = "jw", p = 0.1, ...){
  dt <- setDT(list(id = id, name = name))

  if (n>0){ 
    f <- function(x){
      g <- function(x){
          x <- unique(unlist(str_split(x, "\\s+")))
          out <- NULL
          for (i in seq_len(min(length(x), n))){
            out <- c(out, combn(x, i, function(x){paste(sort(x), collapse = " ")}))
          }
          out
      }
      out <- unlist(lapply(x, g))
      unname(out)
    } 
    dt <- dt[, list(name = f(name)), by = "id"]
  }
  dt <- unique(dt, by = c("id","name"))
  # remove those that are duplicated
  aux <- dt[, .I[.N>1], by = "name"]$V1
  dt2 <- dt[setdiff(seq_len(nrow(dt)), aux)]

  setkey(dt2, id)
  f <- function(x, ...){
    group <- dt2[id==x, name]
    other <- dt2[id!=x, name]
    m <- stringdistmatrix(group, other, method = method, p = p, ...)
    x <- m[,1]
    for (i in 2:ncol(m)) x <- pmin(x, m[,i])
    list(group, x)
  }
  ans <- data.table(id = unique(dt2)$id)
  ans <- ans[, f(id, ...), by = id]
  setnames(ans, c("id", "name", "distance"))
  dt3 <- dt[aux]
  dt3[, distance := 0]
  ans <- rbind(ans, dt3)
  setkey(ans, id, name, distance)
  setDF(ans)
  ans
}






