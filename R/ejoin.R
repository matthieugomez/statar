#ejoin(DTm,DTu,1:1,all=T,gen="_merge")
# The variable "_merge" is 3 for matching rows, 2 for rows in using only, 1 for rows in master only 
# 1:1 m:1 1:m m:m are about the number of rows in each master that should match rows in the other. Great to notice that you have duplicates when you thought you did not
#ejoin(DTm,DTu,m:1,all=T,gen="_merge")
#ejoin(DTm,DTu,1:1,all=T,nogen=T)
#ejoin(DTm,DTu,m:m,all.x=T)
#ejoin(DTm,DTu,m:m,all.y=T)

ejoin=function(DTm,DTu,type,all=FALSE,all.x=all,all.y=all,nogen=FALSE,gen="merge"){
  if (!is.data.table(DTm)){
    setDT(DTm)
    message("Master coerced to data.table")
  }
  if (!is.data.table(DTu)){
    setDT(DTu)
    message("Using coerced to data.table")
  }

  typec=paste(as.character(substitute(type)),collapse="")
  if (!nogen){
    if (gen %chin% names(DTm)){
      stop(paste0(gen," alreay exists in master"))
    }
    if (gen %chin% names(DTu)){
      stop(paste0(gen," alreay exists in using"))
    }
  }

  var=intersect(names(DTm),names(DTu))
  message(paste0("Join based on : ",paste(var,collapse=" ")))


  match <- str_match(typec,":(1|m)(1|m)")
  if (is.na(match[1,1])) stop("Third argument must be 1:1,1:m, m:1 or m:m")
  if (match[1,2]=="1"){
    if (anyDuplicated(DTm)){ 
      stop("Variables don't uniquely identify observations in the master dataset")
    }
  }

  if (match[1,3]=="1"){
    if (anyDuplicated(DTu)){ 
      stop("Variables don't uniquely identify observations in the using dataset")
    }
  }

  setkeyv(DTm,cols=var)
  setkeyv(DTu,cols=var)
  message(paste0("Master and using datasets are now keyed by : ",paste(var,collapse=" ")))

if (!nogen){
    idm=tempvar(c(names(DTm),names(DTu),gen))
    DTm1<- copy(DTm)
    DTm1[,c(idm):=1L]
    idu=tempvar(c(names(DTm),names(DTu),gen,idm))
    DTu1<- copy(DTu)
    DTu1[,c(idu):=1L]
}
  DT_output <- merge(DTm1,DTu1,all=all,all.x=all.x,all.y=all.y,allow.cartesian=TRUE)
  if (!nogen){
    DT_output[,c(gen):=3L]
    DT_output[get(idu)==0L,c(gen):=1L]
    DT_output[get(idm)==0L,c(gen):=2L]
    DT_output[,c(idm):=NULL]
    DT_output[,c(idu):=NULL]
    setcolorder(DT_output,c(gen,setdiff(names(DT_output),gen)))
    eval(substitute(print(table(DT_output$v)),list(v=as.name(gen))))
  }
  DT_output
}