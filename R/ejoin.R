
ejoin =  function(DTm, DTu, type, keep = c(c("master","matched","using")), nogen = FALSE, gen = "merge"){
  all.x <- FALSE
  all.y <- FALSE
  if (length(setdiff(keep,c("master","matched","using")))) stop("keep must be a character vector of the form c(\"master\",\"matched\",\"using\")")
  if ("master" %in% keep){
    all.x = TRUE
  }
  if ("using" %in% keep){
    all.y = TRUE
  }
  if (!is.data.table(DTm)){
    setDT(DTm)
    message("Master coerced to data.table")
  }
  if (!is.data.table(DTu)){
    setDT(DTu)
    message("Using coerced to data.table")
  }

  typec <- paste(as.character(substitute(type)), collapse = "")
  if (!nogen){
    if (gen %chin% names(DTm)){
      stop(paste0(gen," alreay exists in master"))
    }
    if (gen %chin% names(DTu)){
      stop(paste0(gen," alreay exists in using"))
    }
  }

  var=intersect(names(DTm), names(DTu))
  message(paste0("Join based on : ", paste(var, collapse = " ")))


  match <- stringr::str_match(typec,":(1|m)(1|m)")
  if (is.na(match[1,1])) stop("Third argument must be 1:1,1:m, m:1 or m:m")
  if (match[1,2] == "1"){
    if (anyDuplicated(DTm)){ 
      stop("Variables don't uniquely identify observations in the master dataset")
    }
  }

  if (match[1,3] == "1"){
    if (anyDuplicated(DTu)){ 
      stop("Variables don't uniquely identify observations in the using dataset")
    }
  }

  setkeyv(DTm, cols = var)
  setkeyv(DTu, cols = var)
  message(paste0("Master and using datasets are now keyed by : ", paste(var, collapse = " ")))

  idm <- tempname_list("temp", c(names(DTm),names(DTu),gen))
  DTm[, c(idm) := 1L]
  idu <- tempname_list("temp", c(names(DTm),names(DTu),gen,idm))
  DTu[, c(idu) := 1L]

  DT_output <- merge(DTm, DTu, all.x= all.x, all.y= all.y, allow.cartesian= TRUE)
  
  if (!"matched" %in% keep){
     eval(substitute(DT_output <- DT_output[!(v1 == 1L & v2 == 1L)],list(v1=as.name(idm),v2=as.name(idu))))
  }

  if (!nogen){
    DT_output[, c(gen) := 3L]
    eval(substitute(DT_output[is.na(v), c(gen) := 1L],list(v=as.name(idu))))
    eval(substitute(DT_output[is.na(v), c(gen) := 2L],list(v=as.name(idm))))
    DT_output[, c(idm) := NULL]
    DT_output[, c(idu) := NULL]
    DTm[, c(idm) := NULL]
    DTu[, c(idu) := NULL]
  }
  DT_output
}