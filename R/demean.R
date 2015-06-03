#' Demean a vector 
#' 
#' @param x A vector, a list of vector, or a data.frame
#' @param fe List of vectors for group (factor, characters or integers)
#' @return A demeaned vector
#' @details This function calls felm::demeanlist after dealing with missing values and converting group variables into factors
#' @return An object of the same type than `x` (ie vector, list or data.frame) where each vector is replaced by its demaned version.
#' @examples       
#' #demean(c(1,2), fe = c(1L,1L))  
#' #demean(c(NA,2), fe = list(c(1L,2L), c(1L,3L)))               
#' #demean(c(1,2), fe = list(c(NA,2L), c(1L,3L)))
#' #demean(list(c(1,2),c(1,4)), fe = list(c(NA,2L), c(1L,3L)))
#' @export
demean <- function(x, fe){
	flag <- ""
	if (is.atomic(x)){
		flag <- "atomic"
	} else if (is.data.frame(x)){
		flag <- "data.frame"
	} 
	x <- as.data.frame(x)
	x_c <- names(x)[!sapply(x, is.double)]
	if (length(x_c)){
		x <- mutate_each_(x, funs(as.double), vars = x_c)
	}
	if (is.atomic(fe)){
		fe <- list(fe)
	}
	fe <- as.data.frame(fe)
	fe_c <- names(fe)[!sapply(fe, is.factor)]
	if (length(fe_c)){
		fe <- mutate_each_(fe, funs(as.factor), vars = fe_c)
	}
	nrow <- nrow(x)
	rows <- complete.cases(x) & complete.cases(fe)
	m <- demeanlist(filter(x, rows), filter(fe, rows))
	out <- lapply(m, function(y){
		out <- rep(NA, nrow)
		out[rows] <- y
		attributes(out) <- NULL
		out
	})
	names(out) <- NULL
	if (flag == "atomic"){
		out <- out[[1]]
	} else if (flag == "data.frame"){
		out <- as.data.frame(out)
	}
	out
}

