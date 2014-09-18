
egen=function(DT,fun,cols,gen,...,i=TRUE,by=NULL){
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
  if (colm) ansvars = setdiff(names(DT), cols) else ansvars = cols
  eval(substitute(DT[i,gen:=lapply(.SD,fun,...),by,.SD=ansvars]))
}

