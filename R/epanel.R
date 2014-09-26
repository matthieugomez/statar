
epanel <- function(DT,cols,time,fun,gen=func){
    if (!is.data.table(DT)){
      setDT(DT)
      message("Data.frame coerced to data.table")
    }
    func <- paste(as.character(substitute(fun)), collapse = "")
    timevar <- as.character(substitute(time))
    colsub <- substitute(cols)
    colvars <- idvars_q(colsub,names(DT))
    if (eval(substitute(!nrow(DT[is.na(t)]) == 0, list(t = as.name(timevar))))) stop(paste(timevar,"should not have missing values"))
    setkeyv(DT, c(colvars, timevar))
    if (anyDuplicated(DT)) stop(paste(colvars, timevar, "do not identify observations"))
    if (func == "fill"){
        eval(substitute(
            ans <- DT[, list(seq.int(t[1L], t[.N])), by = colvars],
            list(t = as.name(timevar))
            ))
        setnames(ans, c(colvars, timevar))
        setkeyv(ans, c(colvars, timevar))
        return(DT[ans])
    }
    match <- stringr::str_match(func,"(L|F)([0-9]*)\\.(.*)")
     if (!is.na(match[1,1])){
        if (gen %in% names(DT)) stop(paste0(gen," already exists in the data.table"))
        valuevar <- match[1,4]
        DT1 <- DT[, c(colvars, timevar, valuevar), with = FALSE]
        if (match[1,2] == "L") operation <- "+" else operation <- "-"
        operation <- parse(text=paste0(timevar, operation, match[1,3]))
        DT1[,(timevar) := eval(operation)]
        DT[DT1,(gen) := eval(parse(text=paste0("i.", valuevar)))]
        if (!gen %chin% names(DT)) DT[, (gen) := NA]
    }
}

