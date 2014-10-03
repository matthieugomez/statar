#' Join two data.tables together (similar to Stata command joinby)
#' 
#' @param DTm The master data.table
#' @param DTu The using data.table
#' @param type  A character. "m:m" many to many (all pairwise combinations), "1:1" one to one merge, "m:1" many to one merge, "1:m" one to many. Specifies 1 at the rhs or lhs checks that indeed variables uniquely identify observations.
#' @param keep A character vector that specifies rows to keep
#' @param gen Name of new variable to mark result, or the boolean FALSE (default) if no such variable should be created. The variable equals 1 for rows in master only, 2 for rows in using only, 3 for matched rows.
#' @return A data.table that joins rows in master and using datases. Matching is done on common names. The data.table master and using are sorted in place. 
#' @examples
#'  # the option keep specifies rows to keep
#'  ## inner join
#'  ejoin(DTm, DTu, keep = "matched", gen = FALSE)
#'  ## left join
#'  ejoin(DTm, DTu, keep = c("master","matched"), gen = FALSE)
#'  ## full outer join
#'  ejoin(DTm, DTu, keep = c("master","matched","using"), gen = FALSE)
#'   # the option type specifies whether datasets have duplicates with respect to matching variables
#'  ejoin(DTm, DTu, type = "1:1")
#'  ejoin(DTm, DTu, "m:1")
#' @export
ejoin =  function(DTm, DTu, type = c("m:m","m:1","1:m","1:1"), keep = c(c("master","matched","using")), gen = FALSE){
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
    stop(paste0("Master is not a data.table. Convert it first using setDT()"))
  }
  if (!is.data.table(DTu)){
    stop(paste0("Using is not a data.table. Convert it first using setDT()"))
  }

  if (!gen == FALSE){
    if (gen %chin% names(DTm)){
      stop(paste0(gen," alreay exists in master"))
    }
    if (gen %chin% names(DTu)){
      stop(paste0(gen," alreay exists in using"))
    }
  }

  var=intersect(names(DTm), names(DTu))
  message(paste0("Join based on : ", paste(var, collapse = " ")))



  if (type[1] == "1"){
    if (anyDuplicated(DTm)){ 
      stop("Variables don't uniquely identify observations in the master dataset")
    }
  }

  if (type[3] == "1"){
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

  if (!gen == FALSE){
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