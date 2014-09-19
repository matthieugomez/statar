
epanel=function(DT,id,time,fun,gen=func){
    func=paste(as.character(substitute(fun)),collapse="")
    timevar=as.character(substitute(time))
    idsub = substitute(id)
    if (is.call(idsub) && idsub[[1L]] == "-") {
        idm = TRUE
        id = eval(idsub[[2L]], parent.frame(), parent.frame())
    } else idm = FALSE
    if (!is.character(id)) stop("ids should be column numbers or names")
    if (any(is.na(id))) stop("Some items of ids are NA)")
    if (any(!id %chin%  names(DT))){
        #try wildcard 
        if (length(id) !=1L) stop("Some items of ids are not column names")
        id_vector <- strsplit(id, "\\s+")[[1]]
        id=NULL
         for (c in id_vector){
            temp <- grep(glob2rx(c),names(DT),value=TRUE)
            if (!length(temp)) stop("Some items of ids are not column names")
            id <- c(id,temp)
        }
    }
    if (idm) idvars = setdiff(names(DT), id) else idvars = id
    if (func=="fill"){
        setkeyv(DT,c(idvars,timevar))
        eval(substitute(
            ans <- DT[, list(seq.int(t[1L], t[.N])), by = idvars],
            list(t=as.name(timevar))
            ))
        setnames(ans,c(idvars,timevar))
        setkeyv(ans,c(idvars,timevar))
        return(DT[ans])
    }
    match <- str_match(func,"(L|F)([0-9]*)\\.(.*)")
     if (!is.na(match[1,1])){
        valuevar=match[1,4]
        setkeyv(DT,c(idvars,timevar))
        DT1 <- DT[,c(idvars,timevar,valuevar), with=FALSE]
        if (match[1,2]=="L") operation="+" else operation="-"
        operation= parse(text=paste0(timevar,operation,match[1,3]))
        DT1[,(timevar):= eval(operation)]
        DT[DT1,(gen):=eval(parse(text=paste0("i.",valuevar)))]
        if (!gen %chin% names(DT)) DT[,(gen):=NA]
    }
}

