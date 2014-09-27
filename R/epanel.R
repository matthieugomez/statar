#' Set of commands for panel data 
#' 
#' @param DT A data.table.
#' @param cols A character vector corresponding to the panel id
#' @param time A character vector corresponding to time
#' @param cmd Commmand to use. Can be of the form L3.varlue or fill
#' @param inplace A boolean that specifies whether the dataset should be modified in place or not (option is only available wheb creating lagged/forward variables)
#' @examples
#' DT <- data.table(
#'  id = c(1, 1, 1, 1, 1, 2, 2), 
#'  date = c(1992, 1989, 1991, 1990, 1994, 1992, 1991), 
#'  value = c(4.1, 4.5, 3.3, 5.3, 3.0, 3.2, 5.2)
#')
#' DT <- DT %>% epanel(cols = "id", t = "date", L1.value, inplace = FALSE)
#' DT %>% epanel(cols = "id", t = "date", L3.value, gen = "L3.value", inplace = TRUE)
#' DT <- DT %>% epanel(cols = "id", t = "date", fill)
#' @export
epanel <- function(DT, cols, time, cmd, gen = cmdc, inplace = FALSE){
    if (!is.data.table(DT)){
      message("Input is not a data.table")
    }
    cmdc <- paste(as.character(substitute(cmd)), collapse = "")
    timevar <- as.character(substitute(time))
    colsub <- substitute(cols)
    colvars <- idvars_q(colsub,names(DT))
    if (eval(substitute(!nrow(DT[is.na(t)]) == 0, list(t = as.name(timevar))))) stop(paste(timevar,"should not have missing values"))
    setkeyv(DT, c(colvars, timevar))
    if (anyDuplicated(DT)) stop(paste(colvars, timevar, "do not identify observations"))
    if (cmdc == "fill"){
        eval(substitute(
            ans <- DT[, list(seq.int(t[1L], t[.N])), by = colvars],
            list(t = as.name(timevar))
            ))
        setnames(ans, c(colvars, timevar))
        setkeyv(ans, c(colvars, timevar))
        return(DT[ans])
    }
    match <- stringr::str_match(cmdc,"(L|F)([0-9]*)\\.(.*)")
     if (!is.na(match[1,1])){
        if (gen %in% names(DT)) stop(paste0(gen," already exists in the data.table"))
        if (!inplace) DT <- copy(DT)
        valuevar <- match[1,4]
        DT1 <- DT[, c(colvars, timevar, valuevar), with = FALSE]
        if (match[1,2] == "L") operation <- "+" else operation <- "-"
        operation <- parse(text=paste0(timevar, operation, match[1,3]))
        DT1[,(timevar) := eval(operation)]
        DT[DT1,(gen) := eval(parse(text=paste0("i.", valuevar)))]
        if (!gen %chin% names(DT)) DT[, (gen) := NA]
    }
}

