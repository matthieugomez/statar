#' Join two data.tables together 
#' 
#' @param DTm The master data.table
#' @param DTu The using data.table
#' @param type The type of (SQL) join among "outer" (default), "left", "right", "inner", "semi" and "anti"
#' @param gen Name of new variable to mark result, or the boolean FALSE (default) if no such variable should be created. The generated variable equals 1 for rows in master only, 2 for rows in using only, 3 for matched rows.
#' @param check A character that checks for the presence of duplicates. "m:m" many to many (all pairwise combinations), "1:1" one to one merge, "m:1" many to one merge, "1:m" one to many. Specifying 1 at the rhs or lhs checks that joined variables uniquely identify observations in the master or using dataset.
#' @return A data.table that joins rows in master and using datases. Matching is done on common names. Keys are set onmaster and using  data.tables.
#' @examples
#'  join(DTm, DTu, type = "outer", gen = FALSE, check = "m:m")
#' @export
join =  function(DTm, DTu, type = "outer" , gen = FALSE, check =  c("m:m", "m:1", "1:m", "1:1")){

  type <- match.arg(type, c("outer", "left", "right", "inner", "cross", "semi", "anti"))
  if (type == "cross"){
    stop("cross join is not implemented yet")
  }

  if (!is.data.table(DTm)){
    stop(paste0("Master is not a data.table. Convert it first using setDT()"))
  }
  if (!is.data.table(DTu)){
    stop(paste0("Using is not a data.table. Convert it first using setDT()"))
  }

  vars <- intersect(names(DTm), names(DTu))
  message(paste0("Join based on : ", paste(vars, collapse = " ")))
  setkeyv(DTm, vars)
  setkeyv(DTu, vars)
  message(paste0("Master and using datasets are now keyed by : ", paste(vars, collapse = " ")))

  if (check[1] == "1"){
     if (anyDuplicated(DTm)){ 
       stop("Variables don't uniquely identify observations in the master dataset")
     }
   }

  if (check[3] == "1"){
   if (anyDuplicated(DTu)){ 
     stop("Variables don't uniquely identify observations in the using dataset")
   }
  }

  if (gen != FALSE & !(type %in% c("left", "right", "outer"))){
    stop(" The option gen is only available for left, right and outer joins")
  }

  if (type %in% c("left", "right", "outer", "inner")){
    all.x <- FALSE
    all.y <- FALSE
    if (type == "left"| type == "outer"){
      all.x = TRUE
    }
    if (type == "right" | type == "outer"){
      all.y = TRUE
    }
    if (!gen == FALSE){
      if (gen %chin% names(DTm)){
        stop(paste0(gen," alreay exists in master"))
      }
      if (gen %chin% names(DTu)){
        stop(paste0(gen," alreay exists in using"))
      }
      idm <- tempname_list("temp", c(names(DTm),names(DTu),gen))
      DTm[, c(idm) := 1L]
      idu <- tempname_list("temp", c(names(DTm),names(DTu),gen,idm))
      DTu[, c(idu) := 1L]
    }

    DT_output <- merge(DTm, DTu, all.x= all.x, all.y= all.y, allow.cartesian= TRUE)
    if (!gen == FALSE){
      DT_output[, c(gen) := 3L]
      eval(substitute(DT_output[is.na(v), c(gen) := 1L], list(v = as.name(idu))))
      eval(substitute(DT_output[is.na(v), c(gen) := 2L], list(v = as.name(idm))))
      DT_output[, c(idm) := NULL]
      DT_output[, c(idu) := NULL]
      DTm[, c(idm) := NULL]
      DTu[, c(idu) := NULL]
    }
  } else if (type == "semi"){
    w <- unique(DTm[DTu, which = TRUE, allow.cartesian = TRUE])
    w <- w[!is.na(w)]
    DT_output <- DTm[w]
  } else if (type == "anti"){
    DT_output <- DTm[!DTu, allow.cartesian = TRUE]
  } 
  DT_output
}
