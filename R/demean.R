#' Demean a vector 
#' 
#' @param x Vector to deman
#' @param fe List of vectors for group (factor, characters or integers)
#' @return A demeaned vector
#' @details This function calls felm::demeanlist after dealing with missing values and converting group variables into factors
#' @examples       
#' demean(c(1,2), fe = c(1,1))  
#' demean(c(NA,2), fe = list(c(1,2), c(1,3)))               
#' demean(c(1,2), fe = list(c(NA,2), c(1,3)))
#' @export
demean <- function(x, fe){
	stopifnot(is.atomic(x))
	if (is.double(x)){
		x <- as.double(x)
	}
	if (is.atomic(fe)){
		fe <- list(fe)
	}
	ans <- c(list(x), fe)
	setDT(ans)
	rows <- complete.cases(ans)
	ans <- ans[rows]
	for (v in names(ans)[2:length(ans)]){
		if (!ans[,is.factor(get(v))]) ans[, (v) := as.factor(get(v))]
	}
	m <- unlist(demeanlist(ans[, "V1", with = FALSE], ans[, names(ans)[-1], with = FALSE]))
	out <- rep(NA, length(x))
	out[rows] <- m
	out
}