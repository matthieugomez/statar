#' Demean a vector 
#' 
#' @param x A vector or a list of vector 
#' @param fe List of vectors for group (factor, characters or integers)
#' @return A demeaned vector
#' @details This function calls felm::demeanlist after dealing with missing values and converting group variables into factors
#' @returns A vector if `x` is a vector, a list of vectors if `x` is a list of vectors.
#' @examples       
#' demean(c(1,2), fe = c(1,1))  
#' demean(c(NA,2), fe = list(c(1,2), c(1,3)))               
#' demean(c(1,2), fe = list(c(NA,2), c(1,3)))
#' demean(list(c(1,2),c(1,4)), fe = list(c(NA,2), c(1,3)))
#' @export
demean <- function(x, fe){
	flag <- FALSE
	if (is.atomic(x)){
		flag <- TRUE
		x <- list(x)
	}
	x <- as.data.frame(x)
	setDT(x)
	x_c <- names(x)[!sapply(x, is.double)]
	if (length(x_c)){
		x[, c(x_c) := lapply(.SD, as.double), .SDcols = x_c]
	}
	if (is.atomic(fe)){
		fe <- list(fe)
	}
	fe <- as.data.frame(fe)
	setDT(fe)
	fe_c <- names(fe)[!sapply(fe, is.factor)]
	if (length(fe_c)){
		fe[, c(fe_c) := lapply(.SD, as.factor), .SDcols = fe_c]
	}
	nrow <- nrow(x)
	rows <- complete.cases(x) & complete.cases(fe)
	m <- demeanlist(x[rows], fe[rows])
	out <- lapply(m, function(y){
		out <- rep(NA, nrow)
		out[rows] <- y
		attributes(out) <- NULL
		out
	})
	names(out) <- NULL
	if (flag){
		out <- out[[1]]
	}
	out
}

# demeanlist(c(1,2), list(as.factor(c(1,1))))

# demeanlist(as.matrix(list(c(1,2), c(3,4))), list(as.factor(c(1,1))))