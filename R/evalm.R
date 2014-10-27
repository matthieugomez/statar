#' String and expression interpolation in R
#' @param x any syntactically valid R expression
#' @param env environment in which to evalute the expression enclosed in m()
#' @param inherits Default to FALSE
#' @return quotem captures the (unevaluated) expression given as an argument and replaces any expression starting with \code{$} by the name of its evaluation in env. 
#' evalm is a wrapper for eval(quotem(exp))
#' @examples
#' name="Bob"
#' height=72
#' weight=230
#' quotem("My record indicates you are $height inches tall and weight $weight pounds")
#' quotem("Your body mass index is $(round(703*weight/height^2,1))")
#' newvar <- "temp"
#' within <- "var"
#' quotem("my new variable is $(new$within)")
#' library(data.table)
#' N <- 100
#' DT <- data.table(
#'   id = sample(5, N, TRUE),
#'   v1 = sample(5, N, TRUE),
#'   v2 = sample(1e6, N, TRUE)
#' )
#' myvar <- "v1"
#' byvar <- "id"
#' quotem(DT[, list(`$newvar` = mean(`$myvar`)), by = `$byvar`])
#' evalm(DT[, list(`$newvar` = mean(`$myvar`)), by = `$byvar`])


#' @export
#' @rdname evalm
evalm <- function(x, env = parent.frame(), inherits = FALSE){
  call <- substitute(x)
  call <- quotem_(call, env = env, inherits = inherits)
  eval(call, parent.frame())
}


#' @export
#' @rdname evalm
quotem <- function(x, env = parent.frame(), inherits = FALSE){
  quotem_(substitute(x), env = env, inherits = inherits)
}


quotem_ <- function(x, env = parent.frame(), inherits = FALSE){
    if (x ==""){
      return(x)
    }
    if (is.name(x) | is.symbol(x)){
       return(as.name(substitutem_character(as.character(x))))
    } else if (is.character(x)){
        return(substitutem_character(x, env, inherits = inherits))
    } else{
        out <- NULL
        for (i in 1:length(x)){
            if (!is.null(x[[i]]) & length(x[[i]])){
                x[[i]] <- quotem_(x[[i]], env)
            }
        }
        names <- NULL
        if (x[[1]] == quote(list) | x[[1]] == quote(c)){
            names(x) <- sapply(names(x), function(x){substitutem_character(x, env, inherits = inherits)}, USE.NAMES = FALSE)
        }
        return(x)
    }
}


substitutem_character <- function(x, env = parent.frame(), inherits = FALSE){
  if (!x==""){
    while (regexpr("\\$",x)>0){
      location <- regexpr("\\$",x) 
      x_after <- substring(x, location + 1, nchar(x))
      if (substring(x, location + 1, location + 1)=="("){
          cut <- c(location-1, location + 2, location + find_closing(x_after) -1, location + find_closing(x_after)+1)
        } else{
          ans <- regexpr("^[a-zA-Z]*", x_after)
          cut <- c(location -1, location + 1, location + attr(ans, "match.length"), location + attr(ans, "match.length") + 1)
        }
        y <- substitutem_character(substring(x, cut[2], cut[3]), env = env)
        if (!inherits){
          y <- as.character(eval(parse(text = y)[[1]], envir = env, enclos = env))
        } else{
          y <- as.character(eval(parse(text = y)[[1]], envir = env))
        }        
      x <- paste0(substring(x, 1, cut[1]), y, substring(x, cut[4], nchar(x)))
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
  last
}




