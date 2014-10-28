#' String and expression interpolation
#' @param x any syntactically valid R expression
#' @param env environment in which to evalute the expressions enclosed in patterns. Default to current environement
#' @param inherits Default to FALSE
#' @param pattern pattern to use. Default to \code{$}
#' @return The functions \code{pastem} implements string interpolations, similarly to Stata and Julia. The function \code{quotem} implements expression interpolation. The function \code{evalm} is a wrapper for \code{eval(quotem())} (and therefore corresponds to Julia macro \code{eval}). 
#' @details The functions replaces expressions starting with the \code{pattern} by evaluating them in the environment specified by \code{env} (susbtituting by nothing if not found)
#' Names in \code{...} are also substituted, ie \code{list(`$ok`="`$ok`")} will replace both \code{ok}.
#' @examples
#' height <- 72
#' units <- "inches"
#' weight <- 230
#' a <- "ght"
#' pastem("My record indicates you are $height $(units).",
#'          "Your body mass index is $(round(703*weight/height^2,1))",
#'          "My record indicates you are $(hei$a) inches tall")
#' pastem("You are .(height) inches tall.This is below average", pattern = ".")
#' pastem("You are .(height) inches tall.This is below average", pattern = ".", parenthesis.only = TRUE)
#' library(data.table)
#' N <- 100
#' DT <- data.table(
#'   id = sample(5, N, TRUE),
#'   v1 = sample(5, N, TRUE),
#'   v2 = sample(1e6, N, TRUE)
#' )
#' newvar <- quote(temp)
#' myvar <- quote(v1)
#' byvar <- c("id", "v1")
#' quotem(DT[, list(`$newvar` = mean(`$myvar`)), by = `$byvar`])
#' evalm(DT[, list(`$newvar` = mean(`$myvar`)), by = `$byvar`])

#' @export
#' @rdname evalm
pastem <- function(..., sep = " ", pattern = "$", parenthesis.only = FALSE, env = parent.frame(), inherits = FALSE){
  l <- sapply(list(...), function(x){string_interpolation(x, pattern = pattern, parenthesis.only = parenthesis.only, env = env, inherits = inherits)[[1]]})
  paste(l, collapse = sep)
}

#' @export
#' @rdname evalm
pastem0 <- function(..., pattern = "$"){
  pastem(..., sep = "", pattern = pattern, parenthesis.only = parenthesis.only, env = env, inherits = inherits)
}

string_interpolation <- function(x, pattern = "$", parenthesis.only = FALSE, env = parent.frame(), inherits = FALSE){
  i <- 0
  location <- 0
  y <- NULL
  z <- ""
  if (x!=""){
    z <- x
    if (!parenthesis.only){
      while ((regexpr(pattern, z, fixed = TRUE)>0) && (regexpr(pattern, z, fixed = TRUE) < nchar(x)) && (regexpr(paste0(pattern," "), z, fixed = TRUE) <0)){
        i <- i +1        
        location <- regexpr(pattern, z, fixed = TRUE) 
        x_after <- substring(z, location + 1, nchar(z))
        if (substring(z, location + 1, location + 1)=="("){
            cut <- c(location-1, location + 2, location + find_closing_parenthesis(x_after) -1, location + find_closing_parenthesis(x_after)+1)
          } else{
            ans <- regexpr("^[a-zA-Z]*", x_after)
            cut <- c(location -1, location + 1, location + attr(ans, "match.length"), location + attr(ans, "match.length") + 1)
          }
          y <- quotem_character(substring(z, cut[2], cut[3]), env = env)
          y <- eval_character(y, env = env, inherits = inherits)
          z <- paste0(substring(z, 1, cut[1]), y, substring(z, cut[4], nchar(z)))
      }
    } else{
      while (regexpr(paste0(pattern,"("), z, fixed = TRUE)>0){
        i <- i +1        
        location <- regexpr(paste0(pattern,"("), z, fixed = TRUE)
        x_after <- substring(z, location + 1, nchar(z))
        cut <- c(location-1, location + 2, location + find_closing_parenthesis(x_after) -1, location + find_closing_parenthesis(x_after)+1)
          y <- quotem_character(substring(z, cut[2], cut[3]), env = env)
          y <- eval_character(y, env = env, inherits = inherits)
          z <- paste0(substring(z, 1, cut[1]), y, substring(z, cut[4], nchar(z)))
      }
    }
  }
  list(z, i, location, y)
}

eval_character <- function(x, env = parent.frame(), inherits = FALSE){
  x_expr <- parse(text = paste0("`",x,"`"))[[1]]
  if (typeof(x_expr) == "name" | typeof(x_expr) == "symbol"){
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


# Why? because makes sure that byvars considered as a string and not byvars.
#' @export
evalm <- function(x, pattern = "$", parenthesis.only = FALSE, env = parent.frame(), inherits = FALSE){
  call <- expression_interpolation(substitute(x), pattern = pattern, parenthesis.only = parenthesis.only, env = env, inherits = inherits)
  eval(call, parent.frame())
}

#' @export
#' @rdname evalm
quotem <- function(x, pattern = "$", parenthesis.only = FALSE, env = parent.frame(), inherits = FALSE){
  expression_interpolation(substitute(x), pattern = pattern, parenthesis.only = parenthesis.only, env = env, inherits = inherits)
}

#' @export
#' @rdname evalm
expression_interpolation  <- function(x = "", pattern = "$", parenthesis.only = FALSE, env = parent.frame(), inherits = FALSE){
    if (x == ""){
      return(x)
    } else if (is.name(x) | is.symbol(x)){
       out <- string_interpolation(remove_back_quotes(as.character(x)), pattern = pattern, parenthesis.only = parenthesis.only, env = env, inherits = inherits)
       if (out[[2]]==1 && out[[3]]==1){
        # case where the macro is the whole symbol. then the type is directly the type of the object it refers too
          return(out[[4]])
        } else{
        # case where the macro is one part of the symbol. then its type is coerced to expression
           return(as.name(out[[1]]))
        }
    } else if (is.language(x)){
        out <- NULL
        for (i in 1:length(x)){
            if (!is.null(x[[i]]) & length(x[[i]])){
                x[[i]] <- expression_interpolation(x[[i]], pattern = pattern, parenthesis.only = parenthesis.only, env = env, inherits = inherits)
            }
        }
        names <- NULL
        names(x) <- sapply(names(x), function(x){string_interpolation(remove_back_quotes(x), pattern = pattern, parenthesis.only = parenthesis.only, env = env , inherits = inherits)[[1]]}, USE.NAMES = FALSE)
        return(x)    
    #} else if (is.character(x)){   # Do I really want that?
    #  return(sapply(x, function(x){string_interpolation(x, pattern = pattern, parenthesis#.only = parenthesis.only, env = env, inherits = inherits)[[1]]}, USE.NAMES = #FALSE))
    } else{
      # for instance 1
        return(x)
    }
}


remove_back_quotes <- function(x){
  position <- gregexpr("`", x)[[1]]
  if (position[1]== 1 & position[length(position)]==nchar(x)){
    x <- substring(x, 2, nchar(x)-1)
  }
  x
}


