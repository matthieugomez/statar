#' Create unique names within a list, a data.frame, or an environment
#'
#' @param prefix A character vector that specifies prefix for new name
#' @param where A chracter vector, list or an environment
#' @param inherits  Should the name unique also in the enclosing frames of the environment?
#' @param n An integar that specifies length of the output
#' @examples
#' tempname(c("temp1", "temp3"), 4)
#' tempname(globalenv())
#' tempname(data.frame(temp = 1), n = 3)
#' @export
tempname=function(where = globalenv() , n = 1, prefix = ".temp", inherits=TRUE) {
    all_names <- NULL
    i <- 0L
    name <- prefix
    while (n>0){
        i <- i + 1L
        if (is.character(where)){
            while (name %in% where) {
                name <- paste0(prefix, as.character(i))
                i <- i + 1L
            }
        all_names <- c(all_names, name)
        name <- paste0(prefix, as.character(i))
        n <- n-1
        } else{
            while (exists(name, where = where, inherits = inherits)){
            	   name <- paste0(prefix, as.character(i))
                   i <- i + 1L
        	}
            all_names <- c(all_names, name)
            name <- paste0(prefix, as.character(i))
            n <- n-1
        }
    }
    all_names
}




