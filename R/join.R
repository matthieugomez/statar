#' Join two data frames together 
#' 
#' @param x The master data.frame
#' @param y The using data.frame
#' @param on Character vectors specifying variables to match on. Default to common names between x and y. 
#' @param kind The kind of (SQL) join among "full" (default), "left", "right", "inner", "semi", "anti" and "cross". 
#' @param suffixes A character vector of length 2 specifying suffix of overlapping columns. Defaut to ".x" and ".y".
#' @param check A formula checking for the presence of duplicates. Specifying 1~m (resp m~1, 1~1) checks that joined variables uniquely identify observations in x (resp y, both).
#' @param gen Name of new variable to mark result, or the boolean FALSE (default) if no such variable should be created. The variable equals 1 for rows in master only, 2 for rows in using only, 3 for matched rows.
#' @param inplace A boolean. In case "kind"= "left" and RHS of check is 1, the merge can be one in-place. 
#' @param update A boolean. For common variables in x and y not specified in "on", replace missing observations by the non missing observations in y. 
#' @param type Deprecated
#' @return A data.frame that joins rows in master and using datases. Importantly, if x or y are not keyed, the join may change their row orders.
#' @examples
#' library(dplyr)
#' x <- data.frame(a = rep(1:2, each = 3), b=1:6)
#' y <- data.frame(a = 0:1, bb = 10:11)
#' join(x, y, kind = "full")
#' join(x, y, kind = "left", gen = "_merge")
#' join(x, y, kind = "right", gen = "_merge")
#' join(x, y, kind = "inner", check = m~1)
#' join(x, y, kind = "semi")
#' join(x, y, kind = "anti")
#' y <- rename(y, b = bb)
#' join(x, y, kind = "full", on = "a")
#' join(x, y, kind = "full", on = "a", suffixes = c("",".i"))
#' y <- data.frame(a = 0:1, bb = 10:11)
#' join(x, y, kind = "left", check = m~1)
#' x <- data.frame(a = c(1,2), b=c(NA, 2))
#' y <- data.frame(a = c(1,2), b = 10:11)
#' join(x, y, kind = "left", on = "a",  update = TRUE)
#' join(x, y, kind = "left", on = "a", check = m~1,  update = TRUE)

#' @export
join =  function(x, y, kind ,on = intersect(names(x),names(y)), suffixes = c(".x",".y"), check = m~m,  gen = FALSE, inplace = FALSE, update = FALSE, type){

  #kind
  if (!missing(type)){
    warning("type is deprecated, please use the option kind")
    kind <- type
  }

  if (anyDuplicated(names(x))) stop("Duplicate names in x are not allowed")
  if (anyDuplicated(names(y))) stop("Duplicate names in y are not allowed")

  kind <- match.arg(kind, c("full", "left", "right", "inner", "cross", "semi", "anti"))


  # check gen
  if (gen != FALSE & !(kind %in% c("left", "right", "full"))){
    stop(" The option gen only makes sense for left, right and full joins", call. = FALSE)
  }

    # find names and  check no common names
    if (kind == "cross"){
      vars <- character(0)
    } else{
      vars <- on
    }

  #  if (!length(setdiff(names(y), vars))) stop("No column in y beyond the one used in the merge")
    if (!(kind== "semi" | kind == "anti")){
      common_names <- setdiff(intersect(names(x),names(y)), vars)
      if (length(intersect(paste0(common_names, suffixes[1]), setdiff(names(x),common_names)))>0) stop(paste("Adding the suffix",suffixes[1],"in", common_names,"would create duplicates names in x"), call. = FALSE)
      if (length(intersect(paste0(common_names, suffixes[2]), setdiff(names(y),common_names)))>0) stop(paste("Adding the suffix",suffixes[2],"in", common_names,"would create duplicates names in y"), call. = FALSE)
      if (length(common_names)>0){
        for (name in common_names){
          x <- rename(x, !!paste0(name, suffixes[1]) := !!as.name(name))
          y <- rename(y, !!paste0(name, suffixes[2]) := !!as.name(name))
        }
      }
    }
    if (kind == "cross"){
          out <- merge(x, y, by = NULL)
    } else {
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
    if (kind %in% c("left", "right", "full", "inner")){
      if (!gen == FALSE){
        if (gen %in% names(x)){
          stop(paste0(gen," alreay exists in master"))
        }
        if (gen %in% names(y)){
          stop(paste0(gen," alreay exists in using"))
        }
        idm <- tempname(c(names(x), names(y), gen))
        x <- dplyr::mutate(x, !!idm := 1L)
        idu <- tempname(c(names(x), names(y), gen, idm))
        y <- dplyr::mutate(y, !!idu := 1L)
        idm_symbol = quo(!!as.name(idm))
        idu_symbol = quo(!!as.name(idu))
      }
      all.x <- FALSE
      all.y <- FALSE
      if (kind == "left"){
        out <- dplyr::left_join(x, y)
      } else if (kind == "right"){
        out <- dplyr::right_join(x, y)
      } else if (kind == "full"){
        out <- dplyr::full_join(x, y)
      }  else if (kind == "inner"){
        out <- dplyr::inner_join(x, y)
      }

      if (gen != FALSE){
        gen_symbol = quo(!!as.name(gen))
        out <- dplyr::mutate(out, !!gen := 3L)
        out <- dplyr::mutate(out, !!gen := ifelse(is.na(!!idu_symbol), 1, !!gen_symbol))
        out <- dplyr::mutate(out, !!gen := ifelse(is.na(!!idm_symbol), 1, !!gen_symbol))
        out <- dplyr::select_at(out, setdiff(names(out), c(idm, idu)))
      }
    
      if (update){
        for (v in common_names){
          newvx <- paste0(v, suffixes[1])
          newvy <- paste0(v, suffixes[2])
          newvx_symbol = quo(!!as.name(newvx))
          newvy_symbol = quo(!!as.name(newvy))
          out <- dplyr::mutate(out, !!newvx := ifelse(is.na(!!newvx_symbol) & !is.na(!!newvy_symbol), !!newvy_symbol, !!newvx_symbol))
          out <- select_at(out, setdiff(names(out), newvy))
          out <- rename(out, !!v := !!newvx_symbol)
        }
      }
      return(out)
    } else if (kind == "semi"){
        out <- dplyr::semi_join(x, y)
    } else if (kind == "anti"){
        out <- dplyr::anti_join(x, y)
    }
  }
  return(out)
}

