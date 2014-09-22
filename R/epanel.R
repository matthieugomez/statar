
epanel=function(DT,id,time,fun,gen=func){
    func=paste(as.character(substitute(fun)),collapse="")
    timevar=as.character(substitute(time))
    idsub = substitute(id)
    idvars = idvars_q(idsub,names(DT))
    if (eval(substitute(!nrow(DT[is.na(t)])==0,list(t=as.name(timevar))))) stop(paste(timevar,"should not have missing values"))
    if (func=="fill"){
        setkeyv(DT,c(idvars,timevar))
        eval(substitute(
            ans <- DT[list(seq.int(t[1L], t[.N])), by = idvars],
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

