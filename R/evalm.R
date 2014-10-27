#' Substituting expressions enclosed in m() by the symbol of the character they refer to.
#' @param x any syntactically valid R expression
#' @param env environment in which to evalute the expression enclosed in m()
#' @param inherits Default to FALSE
#' @return evalm returns the expression given as an argument, after replacing any expression enclosed in m() by the name of its evaluation in env.
#' @examples
#' name="Bob"
#' height=72
#' weight=230
#' evalm("My record indicates you are m(height) inches tall and weigh m(weight) pounds")
#' evalm("Your body mass index is m(round(703*weight/height^2,1))")
#' newvar <- "temp"
#' within <- "var"
#' evalm("my new variable is m(newm(within))")
#' library(data.table)
#' N <- 100
#' DT <- data.table(
#'   id = sample(5, N, TRUE),
#'   v1 = sample(5, N, TRUE),
#'   v2 = sample(1e6, N, TRUE)
#' )
#' myvar <- "v1"
#' byvar <- "id"
#' evalm(DT[, list(`m(newvar)` = mean(m(myvar))), by = m(byvar)])

#' @export
evalm <- function(x, env = parent.frame(), inherits = FALSE){
  call <- substitute(x)
  call <- substitutem_(call, env = env, inherits = inherits)
  eval(call, parent.frame())
}

#' @export
#' @rdname evalm
substitutem_ <- function(x, env = parent.frame(), inherits = FALSE){
    i <- 0
    if (is.name(x) | is.symbol(x)){
       return(x)
    } else if (is.call(x) && x[[1]]==quote(m)){
      if (exists(as.character(x[[2]]), envir = env, inherits = inherits)){
        get_name <- eval(x[[2]], envir = env, enclos = env)
        return(as.name(get_name))
      } else{
        return("")
      }
    } else if (is.character(x)){
        return(substitutem_character(x, env, inherits = inherits))
    } else{
        out <- NULL
        for (i in 1:length(x)){
            if (!is.null(x[[i]]) & length(x[[i]])){
                x[[i]] <- substitutem_(x[[i]], env)
            }
        }
        names <- NULL
        if (x[[1]] == quote(list)){
            names(x) <- sapply(names(x), function(x){substitutem_character(x, env, inherits = inherits)}, USE.NAMES = FALSE)
        }
        return(x)
    }
}


#' @export
#' @rdname evalm
substitutem <- function(x, env = parent.frame(), inherits = FALSE){
  substitutem_(substitute(x), env = env, inherits = inherits)
}

substitutem_character <- function(x, env = parent.frame(), inherits = FALSE){
  if (!x==""){
    while (regexpr("m\\(",x)>0){
      open <- regexpr("m\\(",x)
      boundary <- find_closing(substring(x, open+1, nchar(x)))+open
      subx <- substring(x, boundary[1]+1, boundary[2]-1)
      y <- substitutem_character(subx, env = env)
      if (!inherits){
        y <- as.character(eval(parse(text = y)[[1]], envir = env, enclos = env))
      } else{
        y <- as.character(eval(parse(text = y)[[1]], envir = env))
      }
      x <- paste0(substring(x, 1, open-1), y, substring(x, boundary[2]+1, nchar(x)))
    }
  }
  x
}


find_closing <- function(x){
  open <- gregexpr("\\(", x)[[1]]
  close <- gregexpr("\\)", x)[[1]]
  first <- open[1]
  new <- rep(0, nchar(x))
  new[open] <- 1
  new[close] <- -1
  new_sum <- cumsum(new)
  last <- which(new_sum==0 & new_sum < c(NA, head(new_sum, -1)))[[1]]
  c(first, last)
}
