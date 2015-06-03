#' Find best string combinations that identify an id
#'
#' @param name a vector of characters
#' @param id a vector of identifiers
#' @param n number of words for combinations. Default to \code{1}.
#' @return \code{tab_accross} returns a data.frame of four columns. The first is id, the second corresponds to unique combination of words in each element of \code{v} with length lower than \code{n} (sorted alphabetically),  the third is the count of these permutation within \code{id}, the fourth is the count of these permutation accross \code{i}. Intuitively, when the count accross group is 1 and the count within group is high, the element can be considered as an identifier of the group.
#' @examples
#' id <- c(1, 1, 2, 2)
#' name <- c("coca cola company", "coca cola incorporated", "apple incorporated", "apple corp")
#' count_combinations(name, id = id)
#' count_combinations(name, id = id, n = 2)
#' @export
count_combinations <- function(name, id, n = 1){
  dt <- setDT(list(id = id, name = name))
  dt <- na.omit(dt, by = "name", cols = "name")
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
  dt <- dt[, list(.N) , by = c("id", "name")]
  setnames(dt, c("id", "name", "count_within"))
  dt[, "count_across" := .N, by = name]
  setorderv(dt, c("id", "count_across", "count_within"), order = c(1,1,-1))
  setDF(dt)
  dt
}



#' Find minimum distance of each word to other groups
#'
#' @param name a vector of characters
#' @param id a vector of identifiers
#' @param n number of words for combinations. Default to \code{0}.
#' @param method See the \code{\link[stringdist]{stringdist}} documentation. Default to \code{"jw"}
#' @param p See  the \code{\link[stringdist]{stringdist}} documentation. Default to \code{0.1}
#' @param ... Other arguments to pass to \code{stringdist}. See the \code{\link[stringdist]{stringdist}} documentation.
#' @return \code{tab_accross} returns a data.frame of four columns. The first is id, the second corresponds to unique combination of words in each element of \code{v} with length lower than \code{n} (sorted alphabetically),  the third is the count of these permutation within \code{id}, the fourth is the count of these permutation accross \code{i}. When the count accross group is 1 and the count within group is high, the element can be considered as an identifier of the group.
#' id <- c(1, 1, 2, 2)
#' name <- c("coca cola company", "coca cola incorporated", "apple incorporated", "apple corp")
#' compute_distance(name, id, n = 0)
#' compute_distance(name, id, n = 1)
#' compute_distance(name, id, n = 2)
#' @export
compute_distance <- function(name, id, n = 1, method = "jw", p = 0.1, ...){
  dt <- setDT(list(id = id, name = name))
  dt <- na.omit(dt, by = "name")
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
  h <- function(x, ...){
    group <- dt2[id==x, name]
    other <- dt2[id!=x, name]
    m <- stringdistmatrix(group, other, method = method, p = p, ...)
    x <- m[,1]
    for (i in 2:ncol(m)) x <- pmin(x, m[,i])
    list(group, x)
  }
  ans <- data.table(id = unique(dt2)$id)
  ans <- ans[, h(id, ...), by = id]
  setnames(ans, c("id", "name", "distance"))
  dt3 <- dt[aux]
  dt3[, "distance" := 0]
  ans <- rbind(ans, dt3)
  setkeyv(ans, c("id", "name", "distance"))
  setDF(ans)
  ans
}






