
eset=function(DT,fun,cols=names(DT),...,i=NULL,by=NULL,d=FALSE){
  func=as.character(substitute(fun))
  func <-match.arg(func,c("sort","order","rename","summarize","balance"))
  options=eval(substitute(alist(...)))
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
  if (func=="sort"){
    eval(substitute(setkeyv(DT,ansvars,...)))
  }
  if (func=="order"){
    eval(substitute(setcolorder(DT,c(ansvars,setdiff(names(DT),ansvars),...))))
  }
  if (func=="rename"){
    eval(substitute(setnames(DT,ansvars,...)))
  }
  if (func=="keep"){
    if (colm) ansvars = setdiff(names(DT), ansvars) else ansvars = ansvars
      DT[,(ansvars):=NULL] 
  }

  if (func=="summarize"){
      eval(substitute(DT[,describe(.SD,d),.SDcols=ansvars,...]))
  }

  if (func=="balance"){
  
  }
}
