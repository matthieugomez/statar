#' Find best string combinations that identify an id
#'
#' @param name a vector of characters
#' @param id a vector of identifiers
#' @param n number of words for combinations. Default to \code{1}.
#' @return \code{tab_accross} returns a data.frame of four columns. The first is id, the second corresponds to unique combination of words in each element of \code{v} with length lower than \code{n} (sorted alphabetically),  the third is the count of these permutation within \code{id}, the fourth is the count of these permutation accross \code{i}. Intuitively, when the count accross group is 1 and the count within group is high, the element can be considered as an identifier of the group.
#' @examples
#' library(stringdist)
#' id <- c(1, 1, 2, 2)
#' name <- c("coca cola company", "coca cola incorporated", "apple incorporated", "apple corp")
#' count_combinations(id, name)
#' count_combinations(id, name, n = 2)
#' @export
count_combinations <- function(id, name, n = 1){
  try_require("stringdist")
  df <- data_frame(id = id, name = name)
  df <- filter(df, !is.na(name))
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
    suppressWarnings(df <- df %>% group_by(id) %>% do_(~data.frame(name = f(.$name))))
  }
  df <- count(df, id, name)
  df <- setNames(df, c("id", "name", "count_within"))
  df <- df %>% group_by(name) %>% mutate(count_across = n())
  arrange_(df, ~id, ~count_across, ~desc(count_within))
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
#' library(stringdist)
#' id <- c(1, 1, 2, 2)
#' name <- c("coca cola company", "coca cola incorporated", "apple incorporated", "apple corp")
#' compute_distance(id, name, n = 0)
#' compute_distance(id, name, n = 1)
#' compute_distance(id, name, n = 2)
#' @export
compute_distance <- function(id, name, n = 1, method = "jw", p = 0.1, ...){
  try_require("stringdist")
  df <- data_frame(id = id, name = name)
  df <- filter(df, !is.na(name))
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
    suppressWarnings(df <- df %>% group_by(id) %>% do_(~data.frame(name = f(.$name))))
  }
  df <- distinct(df, id, name)
  # remove those that are duplicated
  df <- df %>% group_by(name) %>% mutate(n = n()) 

  ans1 <- df %>% filter(n > 1) %>% mutate(distance = 0) %>% select_(~-n)

  ans2 <- df %>% filter(n == 1) %>% select_(~-n)
  uniqueid <- unique(ans2$id)
  h <- function(x, ...){
    group <- ans2 %>%  filter(id == x)
    other <-  ans2 %>%  filter(id != x)
    m <- stringdist::stringdistmatrix(group$name, other$name, method = method, p = p, ...)
    x <- m[,1]
    for (i in 2:ncol(m)) x <- pmin(x, m[,i])
    out <- bind_cols(group, data.frame(distance = x))
  }
  result <- lapply(uniqueid, h)
  result <- rbind_all(result)
  setNames(result, c("id", "name", "distance"))
  ans <- bind_rows(result, ans1)
  arrange_(ans,  ~id , ~name, ~distance)
}






