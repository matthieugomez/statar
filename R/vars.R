#' select variables in a data.frame
#'
#' @param x a data.frame
#' @param ...
#' @return a character vector. All the syntax available in \code{select_vars} are allowed. Characters are understood as wildcards.
#' @examples
#' df <- data.frame(id = 3, id2 = 4)
#' vars(df, id)
#' vars(df, "id*")
#' @export
vars <- function(x, ...){
	dots <- lazy_dots(...)
	out <- sapply(dots, function(z){
		if (is.character(z$expr)){
			return(names(x)[which(str_detect(names(x), glob2rx(z$expr)))])
		} else{
			select_vars_(names(x),  args = z)
		}
	})
	names(out) <- NULL
	out <- unlist(out)
	names(out) <- NULL
	out
}


#' @export
vars_not <- function(x, ...){
	setdiff(names(x), vars(x, ...))
}
