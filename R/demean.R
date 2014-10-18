#' Demean a set of variables in a data.table
#' 
#' @param x A data.table
#' @param ... variables to demean
#' @param fe List of groups to use
#' @return data.table with demeaned variables (NA if one of the variable is missing), with suffix "_demean"
#' @details This function calls felm::demeanlist after checking for missing values and converting group variables into factors
#' @examples                        
#' N=1e3; K=100
#' set.seed(1)
#' DT <- data.table(
#'   id = sample(5, N, TRUE),
#'   v1 =  sample(5, N, TRUE),                          
#'   v2 =  sample(1e6, N, TRUE),                        
#'   v3 =  sample(round(runif(100,max=100),4), N, TRUE) 
#' )
#' demean(DT, list(v2,v3), fe = list(id,v1))

#' @export
demean <- function(x, ..., fe, suffix = "_demean", inplace = FALSE){
	  demean_(x, .dots = lazy_dots(...), fe = substitute(fe), suffix = suffix)
	}

demean_ <- function(x, ..., .dots, fe, suffix = "_demean", inplace = FALSE){
	stopifnot(is.data.table(x))
	dots <- all_dots(.dots, ...)
	vars <- names(select_vars_(names(x), dots))
	fe <- names(select_vars_(names(x), fe))
	ans <- x[, c(vars, fe), with = FALSE]
	rows <- rowSums(as.matrix(is.na(ans)))==0
	for (v in vars){
		if (!ans[,is.double(get(v))]) ans[, (v) := as.double(get(v))]
	}
	for (v in fe){
		if (!ans[,is.factor(get(v))]) ans[, (v) := as.factor(get(v))]
	}
	m <- demeanlist(ans[, vars, with = FALSE], ans[, fe, with = FALSE])
	if (!inplace){
		x <- copy(x)
	}
	x[rows, paste(vars,suffix,sep ="_") := as.list(m)]
}