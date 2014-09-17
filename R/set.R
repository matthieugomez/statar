set=function(DT,fun,cols=names(DT),i=NULL,by=NULL,...,newcols=NULL){
  func=as.character(substitute(fun))
  colsc=unlist(str_split(cols,pattern=" "))
  colscc=NULL
  for (cc in colsc){
    colscc=c(colscc,names(DT)[which(str_detect(names(DT),glob2rx(cc)))])
  }
  if (func=="sort"){
    eval(substitute(setkeyv(DT,colscc,...)))
  }
  if (func=="order"){
    eval(substitute(setcolorder(DT,c(colscc,setdiff(names(DT),colscc),...))))
  }
  if (func=="rename"){
    stopifnot(length(cols)==length(newcols) & !is.element(newcols,names(DT))==FALSE)
    eval(substitute(setnames(DT,colscc,newcols,...)))
  }
  if (func=="balance"){
    setkeyv(DT,c(by,colsc))
    ans <- DT[, seq.int(get(colscc)[1L], get(colscc)[.N]), by = by]
    setnames(ans,"V1",colscc)
    setkeyv(ans,c(by,colscc))
    DT[ans,allow.cartesian=TRUE]
  }
}
