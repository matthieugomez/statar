#' Join two data.tables together 
#' 
#' @param x The master data.table
#' @param y The using data.table
#' @param on Character vectors specifying variables to match on. Default to common names between x and y. 
#' @param type The type of (SQL) join among "outer" (default), "left", "right", "inner", "semi", "anti" and "cross". 
#' @param suffixes A character vector of length 2 to apply to overlapping columns. Defaut to ".x" and ".y".
#' @param check A formula checking for the presence of duplicates. Specifying 1~m (resp m~1, 1~1) checks that joined variables uniquely identify observations in x (resp y, both).
#' @param gen Name of new variable to mark result, or the boolean FALSE (default) if no such variable should be created. The variable equals 1 for rows in master only, 2 for rows in using only, 3 for matched rows.
#' @param inplace A boolean. In case "type"= "left" and RHS of check is 1, the merge can be one in-place. 
#' @param update A boolean. For common variables in x and y not specified in "on", replace missing observations by the non missing observations in y. 
#' @return A data.table that joins rows in master and using datases. Importantly, if x or y are not keyed, the join may change their row orders.
#' @examples
#' library(data.table)
#' x <- data.table(a = rep(1:2, each = 3), b=1:6)
#' y <- data.table(a = 0:1, bb = 10:11)
#' join(x, y, type = "outer")
#' join(x, y, type = "left", gen = "_merge")
#' join(x, y, type = "right", gen = "_merge")
#' join(x, y, type = "inner", check = m~1)
#' join(x, y, type = "semi")
#' join(x, y, type = "anti")
#' setnames(y, "bb", "b")
#' join(x, y, on = "a")
#' join(x, y, on = "a", suffixes = c("",".i"))
#' y <- data.table(a = 0:1, bb = 10:11)
#' join(x, y, type = "left", check = m~1, inplace = TRUE)
#' x <- data.table(a = c(1,2), b=c(NA, 2))
#' y <- data.table(a = c(1,2), b = 10:11)
#' join(x, y, type = "left", on = "a",  update = TRUE)
#' join(x, y, type = "left", on = "a", chec = m~1, inplace = TRUE,  update = TRUE)

#' @export
join =  function(x, y, on = intersect(names(x),names(y)), type = "outer" , suffixes = c(".x",".y"), check = m~m,  gen = FALSE, inplace = FALSE, update = FALSE){

  #type
  type <- match.arg(type, c("outer", "left", "right", "inner", "cross", "semi", "anti"))

  if (!is.data.table(x)){
    stop(paste0("Master is not a data.table. Convert it first using setDT()"))
  }
  if (!is.data.table(y)){
    stop(paste0("Using is not a data.table. Convert it first using setDT()"))
  }
  
  # check inplace possible
  if (inplace & !((type =="left") & check[[3]]==1)){
      stop("inplace = TRUE but type is not left or formula is not ~1)")
  }

  # check gen
  if (gen != FALSE & !(type %in% c("left", "right", "outer"))){
    stop(" The option gen only makes sense for left, right and outer joins", call. = FALSE)
  }

  if (type == "cross"){
        k <- NULL # Setting the variables to NULL first for CRAN check NOTE
        DT_output <- setkey(x[,c(k=1, .SD)],k)[y[, c(k = 1,.SD)], allow.cartesian = TRUE][,k := NULL]
        return(DT_output)
  } else {

    # find names and  check no common names
    vars <- on
    message(paste0("Join based on : ", paste(vars, collapse = " ")))

    if (!length(setdiff(names(y), vars))) stop("No column in y beyond the one used in the merge")

    common_names <- setdiff(intersect(names(x),names(y)), vars)
    if (length(intersect(paste0(common_names, suffixes[1]), setdiff(names(x),common_names)))>0) stop(paste("Adding the suffix",suffixes[1],"in", common_names,"would create duplicates names in x"), call. = FALSE)
    if (length(intersect(paste0(common_names, suffixes[2]), setdiff(names(y),common_names)))>0) stop(paste("Adding the suffix",suffixes[2],"in", common_names,"would create duplicates names in y"), call. = FALSE)
    if (length(common_names)>0){
      setnames(x, common_names, paste0(common_names, suffixes[1]))
      setnames(y, common_names, paste0(common_names, suffixes[2]))
      on.exit(setnames(y, paste0(common_names, suffixes[2]), common_names))
      if (!inplace){
        on.exit(setnames(x, paste0(common_names, suffixes[1]), common_names), add = TRUE)
      }
    }

    # set keys and check duplicates
    key_x <- key(x)
    key_y <- key(y)
    setkeyv(x, vars)
    setkeyv(y, vars)
  
    on.exit(setkeyv(x, key_x), add = TRUE)
    on.exit(setkeyv(y, key_y), add = TRUE)

    if (check[[2]] == 1){
       if (anyDuplicated(x)){ 
         stop(paste0("Variable(s) ",paste(vars, collapse = " ")," don't uniquely identify observations in x"), call. = FALSE)
       }
     }

    if (check[[3]] == 1){
     if (anyDuplicated(y)){ 
       stop(paste0("Variable(s) ",paste(vars, collapse = " ")," don't uniquely identify observations in y"), call. = FALSE)
     }
    }
    if (type %in% c("left", "right", "outer", "inner")){
      if (!gen == FALSE){
        if (gen %chin% names(x)){
          stop(paste0(gen," alreay exists in master"))
        }
        if (gen %chin% names(y)){
          stop(paste0(gen," alreay exists in using"))
        }
        idm <- tempname(c(names(x),names(y),gen))
        x[, c(idm) := 1L]
        idu <- tempname(c(names(x),names(y),gen,idm))
        y[, c(idu) := 1L]
        on.exit(x[, c(idm) := NULL], add = TRUE)
        on.exit(y[, c(idu) := NULL], add = TRUE) 
      }

      if (inplace){
        lhs = setdiff(names(y), vars)
        v <- lapply(paste0("i.",lhs), as.name)
        call <- as.call(c(quote(list), v)) 
        call <- substitute(x[y,(lhs) := v], list(v = call))
        eval(call)
        if (!gen == FALSE){
          x[, c(gen) := 3L]
          eval(substitute(x[is.na(v), c(gen) := 1L], list(v = as.name(idu))))
          x[, c(idu) := NULL]
        }
        DT_output <- x
      } else{
        all.x <- FALSE
        all.y <- FALSE
        if (type == "left"| type == "outer"){
          all.x = TRUE
        }
        if (type == "right" | type == "outer"){
          all.y = TRUE
        }
        DT_output <- merge(x, y, all.x = all.x, all.y= all.y, allow.cartesian= TRUE)
        if (gen != FALSE){
          DT_output[, c(gen) := 3L]
          eval(substitute(DT_output[is.na(v), c(gen) := 1L], list(v = as.name(idu))))
          eval(substitute(DT_output[is.na(v), c(gen) := 2L], list(v = as.name(idm))))
          DT_output[, c(idm) := NULL]
          DT_output[, c(idu) := NULL]
        }
      }
      if (update){
        for (v in common_names){
          newvx <- paste0(v,suffixes[1])
          newvy <- paste0(v,suffixes[2])
          condition <- DT_output[is.na(get(newvx)) & !is.na(get(newvy)), which = TRUE]
          message(paste("Update of", v, ":", length(condition), "row(s) are updated"))
          DT_output[condition, (newvx) := get(newvy)]
          DT_output[, (newvy) := NULL]
        }
        setnames(DT_output, paste0(common_names, suffixes[1]), common_names)
      }
      DT_output[]
      return(DT_output)
    } else if (type == "semi"){
        w <- unique(x[y, which = TRUE, allow.cartesian = TRUE])
        w <- w[!is.na(w)]
        DT_output <- x[w]
        return(DT_output)
    } else if (type == "anti"){
        DT_output <- x[!y, allow.cartesian = TRUE]
        return(DT_output)
    }
  }
}

