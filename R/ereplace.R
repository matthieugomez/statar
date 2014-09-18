ereplace=function(DT,fun,cols=names(DT),...,i=TRUE,by=NULL){
	func=as.character(substitute(fun))
	colsub = substitute(cols)
	if (is.call(colsub) && colsub[[1L]] == "-") {
	    colm = TRUE
	    cols = eval(colsub[[2L]], parent.frame(), parent.frame())
	} else colm = FALSE
	if (!is.character(cols)) stop("cols should be column numbers or names")
	if (any(is.na(cols))) stop("Some items of cols are NA)")
	if (any(!cols %chin% names(DT))){
	    #try wildcard 
	    if (length(cols) !=1L) stop("Some items of cols are not column names")
	    cols_vector <- strsplit(cols, "\\s+")[[1]]
	    cols=NULL
	     for (c in cols_vector){
	        temp <- grep(glob2rx(c),names(DT),value=TRUE)
	        if (!length(temp)) stop("Some items of cols are not column names")
	        cols <- c(cols,temp)
	    }
	}
	if (colm) colvars = setdiff(names(DT), cols) else colvars = cols



	bysub = substitute(bys)
	if (is.call(bysub) && bysub[[1L]] == "-") {
	    bym = TRUE
	    bys = eval(bysub[[2L]], parent.frame(), parent.frame())
	} else bym = FALSE
	if (!is.character(bys)) stop("bys should be byumn numbers or names")
	if (any(is.na(bys))) stop("Some items of bys are NA)")
	if (any(!bys %chin% names(DT))){
	    #try wildcard 
	    if (length(bys) !=1L) stop("Some items of bys are not byumn names")
	    bys_vector <- strsplit(bys, "\\s+")[[1]]
	    bys=NULL
	     for (c in bys_vector){
	        temp <- grep(glob2rx(c),names(DT),value=TRUE)
	        if (!length(temp)) stop("Some items of bys are not byumn names")
	        bys <- c(bys,temp)
	    }
	}
	if (bym) byvars = setdiff(names(DT), bys) else byvars = bys





  eval(substitute(DT[i,colvars:=lapply(.SD,fun,...),by,.SD=byvars]))
}

