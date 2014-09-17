suppressMessages(library("magrittr"))
suppressMessages(library("data.table"))
suppressMessages(library("dplyr"))
suppressMessages(library("reshape2"))
suppressMessages(library("ggplot2"))
suppressMessages(library("stringr"))
suppressMessages(library("lubridate"))
suppressMessages(library("pryr"))
suppressMessages(library("lfe"))
suppressMessages(library("foreign"))
suppressMessages(library("stargazer"))
suppressMessages(library("parallel"))
suppressMessages(library("magrittr"))
options(mc.cores=3)

old=theme_set(theme_bw()+theme(title=element_text(size=8), axis.title=element_text(size=10),  strip.text.x=element_text(size=8),legend.key = element_blank(), legend.title = element_text(face="plain", size=10), legend.position="bottom", strip.background = element_rect(colour="white", fill="white")))

#sink.reset <- function(){
#    for(i in seq_len(sink.number())){
#        sink(NULL)
#    }
#}
#ftext=element_text(family="CM Roman"),
# axis.text.x=element_text(angle=45,hjust=1,vjust=1, size=6)
# axis.ticks = theme_blank(),axis.text.x = element_blank()
# legend.key.width= unit(1,"cm")
# legend.title=element_text(size=10),legend.position="right"



######## Personal Functions

# stringr
str_select=function(string,pattern){
    string[which(str_detect(string,pattern))]
}
str_drop=function(string,pattern){
  string[-which(str_detect(string,pattern))]
}


## Summary Statistics
set=function(DT,fun,cols=names(DT),i=NULL,by=NULL,...,newcols=NULL){
  func=as.character(substitute(fun))
  colsc=unlist(str_split(cols,pattern=" "))
  colscc=NULL
  for (cc in colsc){
    colscc=c(colscc,names(DT)[which(str_detect(names(DT),glob2rx(cc)))])
  }
  if (func=="sort"){
    eval(substitute(setkeyv(DT,colscc,...)))
  }
  if (func=="order"){
    eval(substitute(setcolorder(DT,c(colscc,setdiff(names(DT),colscc),...))))
  }
  if (func=="rename"){
    stopifnot(length(cols)==length(newcols) & !is.element(newcols,names(DT))==FALSE)
    eval(substitute(setnames(DT,colscc,newcols,...)))
  }
  if (func=="balance"){
    setkeyv(DT,c(by,colsc))
    ans <- DT[, seq.int(get(colscc)[1L], get(colscc)[.N]), by = by]
    setnames(ans,"V1",colscc)
    setkeyv(ans,c(by,colscc))
    DT[ans,allow.cartesian=TRUE]
  }
}



#collapse=function(DT,fun,cols=names(DT),newcols=cols,i=NULL,by=NULL,...){
#  func=as.character(substitute(fun))
#  colsc=unlist(str_split(cols,pattern=" "))
#  cc=NULL
#  for (c in colsc){
#    cc=c(cc,names(DT)[which(str_detect(names(DT),glob2rx(c)))])
#  }
#  colsc= cc
#  eval(substitute(DT[i,lapply(.SD,fun),by,.SD=colsc]))
#}

replace=function(DT,fun,cols=names(DT),i=TRUE,by=NULL,...){
  func=as.character(substitute(fun))
  colsc=unlist(str_split(cols,pattern=" "))
  cc=NULL
  for (c in colsc){
    cc=c(cc,names(DT)[which(str_detect(names(DT),glob2rx(c)))])
  }
  colsc= cc
  if (is.character(fun)|is.numeric(fun)|is.integer(fun)|is.logical(fun)){
      eval(substitute(DT[i,colsc:=lapply(.SD,function(x){fun}),by,.SD=colsc]))
    } else{  
      eval(substitute(DT[i,colsc:=lapply(.SD,function(x){fun(x,...)}),by,.SD=colsc]))
    }
}

gen=function(DT,fun,cols=names(DT),newcols,i=TRUE,by=NULL,...){
  stopifnot(length(cols)==length(newcols) & !is.element(newcols,names(DT))==FALSE)
  func=as.character(substitute(fun))
  colsc=unlist(str_split(cols,pattern=" "))
  cc=NULL
  for (c in colsc){
    cc=c(cc,names(DT)[which(str_detect(names(DT),glob2rx(c)))])
  }
  colsc= cc
  if (is.character(fun)|is.numeric(fun)|is.integer(fun)|is.logical(fun)){
      eval(substitute(DT[i,newcols:=lapply(.SD,function(x){fun}),by,.SD=colsc]))
    } else{  
      eval(substitute(DT[i,newcols:=lapply(.SD,function(x){fun(x,...)}),by,.SD=colsc]))
    }
}


## describe
describe=function(M,details = FALSE,na.rm = TRUE,mc.cores=getOption("mc.cores", 2L)){
  # import from stargazer
  .iround <- function(x, decimal.places = 0, round.up.positive = FALSE, 
      simply.output = FALSE,  .format.digit.separator = ",") {
    .format.initial.zero <- TRUE
    .format.until.nonzero.digit <- TRUE
    .format.max.extra.digits <- 2
    .format.digit.separator.where <- c(3)
    .format.ci.separator <- ", "
    .format.round.digits <- 3
    .format.decimal.character <- "."
    .format.dec.mark.align <- FALSE
    .format.dec.mark.align <- TRUE
      x.original <- x
      first.part <- ""
      if (is.na(x) | is.null(x)) {
          return("")
      }
      if (simply.output == TRUE) {
          if (!is.numeric(x)) {
              return(.remove.special.chars(x))
          }
      }
      if (x.original < 0) {
          x <- abs(x)
      }
      if (!is.na(decimal.places)) {
          if ((.format.until.nonzero.digit == FALSE) | (decimal.places <= 
              0)) {
              round.result <- round(x, digits = decimal.places)
          }
          else {
              temp.places <- decimal.places
              if (!.is.all.integers(x)) {
                while ((round(x, digits = temp.places) == 0) & 
                  (temp.places < (decimal.places + .format.max.extra.digits))) {
                  temp.places <- temp.places + 1
                }
              }
              round.result <- round(x, digits = temp.places)
              decimal.places <- temp.places
          }
          if ((round.up.positive == TRUE) & (round.result < 
              x)) {
              if (x > (10^((-1) * (decimal.places + 1)))) {
                round.result <- round.result + 10^((-1) * decimal.places)
              }
              else {
                round.result <- 0
              }
          }
      }
      else {
          round.result <- x
      }
      round.result.char <- as.character(format(round.result, 
          scientific = FALSE))
      split.round.result <- unlist(strsplit(round.result.char, 
          "\\."))
      for (i in seq(from = 1, to = length(.format.digit.separator.where))) {
          if (.format.digit.separator.where[i] <= 0) {
              .format.digit.separator.where[i] <<- -1
          }
      }
      separator.count <- 1
      length.integer.part <- nchar(split.round.result[1])
      digits.in.separated.unit <- 0
      for (i in seq(from = length.integer.part, to = 1)) {
          if ((digits.in.separated.unit == .format.digit.separator.where[separator.count]) & 
              (substr(split.round.result[1], i, i) != "-")) {
              first.part <- paste(.format.digit.separator, 
                first.part, sep = "")
              if (separator.count < length(.format.digit.separator.where)) {
                separator.count <- separator.count + 1
              }
              digits.in.separated.unit <- 0
          }
          first.part <- paste(substr(split.round.result[1], 
              i, i), first.part, sep = "")
          digits.in.separated.unit <- digits.in.separated.unit + 
              1
      }
      if (x.original < 0) {
          if (.format.dec.mark.align == TRUE) {
              first.part <- paste("-", first.part, sep = "")
          }
          else {
              first.part <- paste("$-$", first.part, sep = "")
          }
      }
      if (!is.na(decimal.places)) {
          if (decimal.places <= 0) {
              return(first.part)
          }
      }
      if (.format.initial.zero == FALSE) {
          if ((round.result >= 0) & (round.result < 1)) {
              first.part <- ""
          }
      }
      if (length(split.round.result) == 2) {
          if (is.na(decimal.places)) {
              return(paste(first.part, .format.decimal.character, 
                split.round.result[2], sep = ""))
          }
          if (nchar(split.round.result[2]) < decimal.places) {
              decimal.part <- split.round.result[2]
              for (i in seq(from = 1, to = (decimal.places - 
                nchar(split.round.result[2])))) {
                decimal.part <- paste(decimal.part, "0", sep = "")
              }
              return(paste(first.part, .format.decimal.character, 
                decimal.part, sep = ""))
          }
          else {
              return(paste(first.part, .format.decimal.character, 
                split.round.result[2], sep = ""))
          }
      }
      else if (length(split.round.result) == 1) {
          if (is.na(decimal.places)) {
              return(paste(first.part, .format.decimal.character, 
                decimal.part, sep = ""))
          }
          decimal.part <- ""
          for (i in seq(from = 1, to = decimal.places)) {
              decimal.part <- paste(decimal.part, "0", sep = "")
          }
          return(paste(first.part, .format.decimal.character, 
              decimal.part, sep = ""))
      }
      else {
          return(NULL)
      }
  }
  is.wholenumber <- function(x, tol = .Machine$double.eps^0.5) abs(x - 
      round(x)) < tol
  .is.all.integers <- function(x) {
      if (!is.numeric(x)) {
          return(FALSE)
      }
      if (length(x[!is.na(x)]) == length(is.wholenumber(x)[(!is.na(x)) & 
          (is.wholenumber(x) == TRUE)])) {
          return(TRUE)
      }
      else {
          return(FALSE)
      }
  }


  # Now starts the code 

  if (details==FALSE) {
   sum_mean <-as.data.frame(mclapply(M ,function(x){c(length(x),sum(is.na(x)),mean(x,na.rm=na.rm),sd(x,na.rm= na.rm),quantile(x,c(0,1),type=1,na.rm=na.rm))}))
    sum <- as.matrix(sum_mean)
    rownames(sum) <- c("N","NA","Mean","Sd","Min","Max")
  } else {
    N <- nrow(M)
    sum_mean <- colMeans(M ,na.rm=na.rm)
    f=function(x,m){
      sum_higher <- colMeans(DT[,list((x-m)^2,(x-m)^3,(x-m)^4)],na.rm=na.rm)
      sum_higher[1] <- sqrt(sum_higher[1])
      sum_higher[2] <- sum_higher[2]/sum_higher[1]^3
      sum_higher[3] <- sum_higher[3]/sum_higher[1]^4
      sum_quantile=quantile(x,c(0,0.01,0.05,0.1,0.25,0.50,0.75,0.9,0.95,0.99,1),type=1,na.rm=na.rm,names=FALSE)
      sum <- c(N,sum(is.na(x)),m,sum_higher,sum_quantile)
    }
    sum <- do.call(cbind,mcMap(f,M,sum_mean))
    rownames(sum) <- c("N","NA","Mean","Sd","Skewness","Kurtosis","Min","1%","5%","10%","25%","50%","75%","90%","95%","99%","Max")
  }
  print <- apply(sum,c(1,2),function(x){
    y <- .iround(x,decimal.places=3)
    y<- str_replace(y,"0+$","")
    if (y==""){
      y <- "0"
    }
    y<- str_replace(y,"\\.$","")
    y<- str_replace(y,"-0","0")


  })
  print(noquote(format(print,justify="right")),right=TRUE)
}





