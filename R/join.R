#' Join two data.tables together 
#' 
#' @param x The master data.table
#' @param y The using data.table
#' @param on Character vectors for variables to match on. Default to common names between x and y. 
#' @param type The type of (SQL) join among "outer" (default), "left", "right", "inner", "semi" and "anti"
#' @param gen Name of new variable to mark result, or the boolean FALSE (default) if no such variable should be created. The generated variable equals 1 for rows in master only, 2 for rows in using only, 3 for matched rows.
#' @param check A character that checks for the presence of duplicates. "m:m" many to many (all pairwise combinations), "1:1" one to one merge, "m:1" many to one merge, "1:m" one to many. Specifying 1 at the rhs or lhs checks that joined variables uniquely identify observations in the master or using dataset.
#' @return A data.table that joins rows in master and using datases. In order to avoid duplicates, identical variable names not joined are renamed with a .x and .y suffixes. Keys are set on master and using data.tables, which avoids the copy of x and y, at the cost of transforming the input data.tables.
#' @examples
#'  join(x, y, on = intersect(names(x),names(y)), type = "outer", gen = FALSE, check = "m:m")
#' @export
join =  function(x, y, on = intersect(names(x),names(y)), type = "outer" , gen = FALSE, check =  c("m:m", "m:1", "1:m", "1:1")){

  #type
  type <- match.arg(type, c("outer", "left", "right", "inner", "cross", "semi", "anti"))
  if (type == "cross"){
    stop("cross join is not implemented yet")
  }

  if (!is.data.table(x)){
    stop(paste0("Master is not a data.table. Convert it first using setDT()"))
  }
  if (!is.data.table(y)){
    stop(paste0("Using is not a data.table. Convert it first using setDT()"))
  }
  
  # check gen
  if (gen != FALSE & !(type %in% c("left", "right", "outer"))){
    stop(" The option gen is only available for left, right and outer joins")
  }

  # join names
  vars <- on
  message(paste0("Join based on : ", paste(vars, collapse = " ")))

  # set keys
  setkeyv(x, vars)
  setkeyv(y, vars)

  # check duplicates
  if (check[1] == "1"){
     if (anyDuplicated(x)){ 
       stop("Variables don't uniquely identify observations in the master dataset")
     }
   }

  if (check[3] == "1"){
   if (anyDuplicated(y)){ 
     stop("Variables don't uniquely identify observations in the using dataset")
   }
  }

  common_names <- setdiff(intersect(names(x),names(y)), vars)
  if (length(intersect(paste0(common_names, ".x"), setdiff(names(x),common_names)))>0) stop(paste("Adding the suffix .x in", common_names,"would create duplicates names in x"))
  if (length(intersect(paste0(common_names, ".y"), setdiff(names(y),common_names)))>0) stop(paste("Adding the suffix .y in", common_names,"would create duplicates names in y"))

  if (length(common)>0){
    x <- copy(x)
    y <- copy(x)
    setnames(x, common_names, paste0(common_names, ".x"))
    setnames(x, common_names, paste0(common_names, ".y"))
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
      if (gen %chin% names(x)){
        stop(paste0(gen," alreay exists in master"))
      }
      if (gen %chin% names(y)){
        stop(paste0(gen," alreay exists in using"))
      }
      idm <- tempname_list("temp", c(names(x),names(y),gen))
      x[, c(idm) := 1L]
      idu <- tempname_list("temp", c(names(x),names(y),gen,idm))
      y[, c(idu) := 1L]
    }

    DT_output <- merge(x, y, all.x = all.x, all.y= all.y, allow.cartesian= TRUE)
    if (gen != FALSE){
      DT_output[, c(gen) := 3L]
      eval(substitute(DT_output[is.na(v), c(gen) := 1L], list(v = as.name(idu))))
      eval(substitute(DT_output[is.na(v), c(gen) := 2L], list(v = as.name(idm))))
      DT_output[, c(idm) := NULL]
      DT_output[, c(idu) := NULL]
      x[, c(idm) := NULL]
      y[, c(idu) := NULL]
    }
  } else if (type == "semi"){
    w <- unique(x[y, which = TRUE, allow.cartesian = TRUE])
    w <- w[!is.na(w)]
    DT_output <- x[w]
  } else if (type == "anti"){
    DT_output <- x[!y, allow.cartesian = TRUE]
  } 
  DT_output
}
