#' String and expression interpolation
#' @param x any syntactically valid R expression
#' @param env environment in which to evalute the expressions enclosed in patterns. Default to current environement
#' @param inherits Default to FALSE
#' @param pattern pattern to use. Default to \code{$}
#' @return The functions \code{quotem} implements expression and string interpolations, similarly to Stata and Julia. \code{quotem} captures the (unevaluated) expression given as an argument and returns the expression obtained by evaluating any expression starting with \code{pattern}. The function \code{evalm} is a wrapper for \code{eval(quotem())} (and therefore corresponds to Julia macro \code{eval})
#' @details The algorithm that replaces expressions starting with the \code{pattern} is the following. If the expression is of type name or symbol, it is replaced by a symbol of the object it is bounded to in \code{env} (or removed if the name does not exists in \code{env}) If the expression is of not a name (like a call), the call is evaluated in the environment specified by \code{env}).
#'
#' Names in \code{...} are also substituted, ie \code{list(`$ok`="`$ok`")} will replace both \code{ok}.
#' @examples
#' height <- 72
#' units <- "inches"
#' weight <- 230
#' quotem("My record indicates you are $height $(units).")
#' quotem("Your body mass index is $(round(703*weight/height^2,1))")
#' quotem("My record indicates you are $(hei$a) inches tall")
#' quotem("You are .(height) inches tall.This is below average", pattern = ".")
#' quotem("You are .(height) inches tall.This is below average", pattern = ".", parenthesis.only = TRUE)
#' a <- "ght"
#' library(data.table)
#' N <- 100
#' DT <- data.table(
#'   id = sample(5, N, TRUE),
#'   v1 = sample(5, N, TRUE),
#'   v2 = sample(1e6, N, TRUE)
#' )
#' newvar <- "temp"
#' myvar <- "v1"
#' byvar <- "id"
#' quotem(DT[, list(`$newvar` = mean(`$myvar`)), by = `$byvar`])
#' evalm(DT[, list(`$newvar` = mean(`$myvar`)), by = `$byvar`])
#' @export
evalm <- function(x, env = parent.frame(), inherits = FALSE, pattern = "$"){
  call <- substitute(x)
  call <- quotem_(call, env = env, inherits = inherits, pattern = "$")
  eval(call, parent.frame())
}


#' @export
#' @rdname evalm
quotem <- function(x, env = parent.frame(), inherits = FALSE, pattern = "$", parenthesis.only = FALSE){
  quotem_(substitute(x), env = env, inherits = inherits, pattern = pattern, parenthesis.only = parenthesis.only)
}


quotem_ <- function(x, env = parent.frame(), inherits = FALSE, pattern = "$", parenthesis.only = FALSE){
    if (x ==""){
      return(x)
    }
    if (is.name(x) | is.symbol(x)){
       y <- substitutem_character(as.character(x), env = env, inherits = inherits, pattern = pattern, parenthesis.only = parenthesis.only)
      if (length(y) >= 2) warnings("A name was bound to a character vector. Only the first character vector if used")
      return(as.name(y))
    } else if (is.character(x)){
        return(substitutem_character(x, env, inherits = inherits, pattern = pattern, parenthesis.only = parenthesis.only))
    } else{
        out <- NULL
        for (i in 1:length(x)){
            if (!is.null(x[[i]]) & length(x[[i]])){
                x[[i]] <- quotem_(x[[i]], env, inherits = inherits, pattern = pattern, parenthesis.only = parenthesis.only)
            }
        }
        names <- NULL
        names(x) <- sapply(names(x), function(x){substitutem_character(x, env, inherits = inherits, pattern = pattern, parenthesis.only = parenthesis.only)}, USE.NAMES = FALSE)
        return(x)
    }
}


substitutem_character <- function(x, env = parent.frame(), inherits = FALSE, pattern = "$", parenthesis.only = FALSE){
  if (!x==""){
    if (!parenthesis.only){
      while ((regexpr(pattern, x, fixed = TRUE)>0) && (regexpr(pattern, x, fixed = TRUE) < nchar(x)) && (regexpr(paste0(pattern," "), x, fixed = TRUE) <0)){
        location <- regexpr(pattern, x, fixed = TRUE) 
        x_after <- substring(x, location + 1, nchar(x))
        if (substring(x, location + 1, location + 1)=="("){
            cut <- c(location-1, location + 2, location + find_closing_parenthesis(x_after) -1, location + find_closing_parenthesis(x_after)+1)
          } else{
            ans <- regexpr("^[a-zA-Z]*", x_after)
            cut <- c(location -1, location + 1, location + attr(ans, "match.length"), location + attr(ans, "match.length") + 1)
          }
          y <- substitutem_character(substring(x, cut[2], cut[3]), env = env)
          y <- eval_character(y, env = env, inherits = inherits)
          x <- paste0(substring(x, 1, cut[1]), y, substring(x, cut[4], nchar(x)))
      }
    } else{
      while (regexpr(paste0(pattern,"("), x, fixed = TRUE)>0){
        location <- regexpr(paste0(pattern,"("), x, fixed = TRUE)
        x_after <- substring(x, location + 1, nchar(x))
        cut <- c(location-1, location + 2, location + find_closing_parenthesis(x_after) -1, location + find_closing_parenthesis(x_after)+1)
          y <- substitutem_character(substring(x, cut[2], cut[3]), env = env)
          y <- eval_character(y, env = env, inherits = inherits)
        x <- paste0(substring(x, 1, cut[1]), y, substring(x, cut[4], nchar(x)))
      }
    }
  }
  x
}

eval_character <- function(x, env = parent.frame(), inherits = FALSE){
  x_expr <- parse(text = x)[[1]]
  if (typeof(x_expr) == "name" | typeof(x_expr) == "symbol"){
    if (exists(x, env, inherits = inherits)){
      return(get(x, env = env, inherits = inherits))
    } else {
      return("")
    }
    if (exists(x, env, inherits = inherits)){
      return(get(x, env = env, inherits = inherits))
    } else {
      return("")
    }
  } else if(typeof(x_expr)== "language"){
        if (inherits){
          return(eval(x_expr, env = env))
        } else{
          return(eval(x_expr, env = env, enclos = env))
        }
  } else {
    return("")
  }
} 

find_closing_parenthesis <- function(x){
  open <- gregexpr("\\(", x)[[1]]
  close <- gregexpr("\\)", x)[[1]]
  first <- open[1]
  new <- rep(0, nchar(x))
  new[open] <- 1
  new[close] <- -1
  new_sum <- cumsum(new)
  last <- which(new_sum==0 & new_sum < c(NA, head(new_sum, -1)))[[1]]
  last
}




