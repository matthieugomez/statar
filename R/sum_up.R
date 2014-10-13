#' Gives summary statistics (corresponds to Stata command summarize)
#' 
#' @param x a data.table
#' @param ... Variables to include. Defaults to all non-grouping variables. See the \link[dplyr]{select} documentation.
#' @param w Weights. Default to NULL. 
#' @param by Groups within which summary statistics are printed. Default to NULL. See the \link[dplyr]{select} documentation.
#' @param d Should detailed summary statistics be printed?
#' @examples
#' N <- 1000; K <- 10
#' DT <- data.table(
#'   id = 1:N,
#'   v1 = sample(5, N, TRUE),
#'   v2 = sample(1e6, N, TRUE)
#' )
#' sum_up(DT)
#' sum_up(DT, v2, d = T)
#' sum_up(DT, starts_with("v"), by = v1)
#' sum_up(DT, by = v1, graph = TRUE)
#' @export
sum_up <- function(x, ...,  d = FALSE, w = NULL, na.rm = TRUE, by = NULL, graph = FALSE, minimal = TRUE) {
  sum_up_(x, .dots = lazy_dots(...) , d = d, w = substitute(w), na.rm = na.rm, by = substitute(by), graph = graph, minimal = minimal)
}


#' @export
#' @rdname sum_up
sum_up_<- function(x, ..., .dots, d = FALSE,  w= NULL, na.rm = TRUE, by = NULL, graph = FALSE, minimal = TRUE) {
  stopifnot(is.data.table(x))
  w <- names(select_vars_(names(x), w))
  if (!length(w)) w <- NULL
  byvars <- names(select_vars_(names(x), by))
  dots <- all_dots(.dots, ...)
  vars <- names(select_vars_(names(x), dots, exclude = byvars))
  if (length(vars) == 0) {
     vars <- setdiff(names(x), c(byvars,w))
  }
  nums <- sapply(x, is.numeric)
  nums_name <- names(nums[nums==TRUE])
  vars=intersect(vars,nums_name)
  if (!length(vars)) stop("Please select at least one non-numeric variable", call. = FALSE)

  if (!is.null(w)){
    w <- x[[which(names(x)== w)]]
  }
  if (graph){
    x <- x[, c(byvars, vars, w), with = FALSE]
    assign_var(x, variable, value, group)
    if (minimal){
      theme = theme_set(theme_minimal())
      theme = theme_update(legend.position="top", legend.title=element_blank(), panel.grid.major.x=element_blank())
      theme = theme_update(axis.text.x=element_blank(), axis.ticks.x = element_blank(), axis.line.x = element_blank(), axis.title.x=element_blank())
      theme = theme_update(axis.line.y = element_blank(), axis.title.y=element_blank(), axis.text.y = element_text(colour="grey"), axis.ticks.y= element_line(colour="grey"))
    }
    if (length(byvars)){
      if (length(byvars)>1){
          setkeyv(x, byvars)
          x[, .group := 0]
          x[unique(x), .group := 1]
          evaldt(x[, .group:= cumsum(.group)])
      } else{
        group <- byvars
      }
      evaldt(x[, .group := as.factor(.group)])
    } else{
      group <- factor(0)
      evaldt(x[, .group := 1])
    }
    if (!length(w)){
      w <- NULL
    }
    x <-  suppressWarnings(suppressMessages(gather_(x, variable, value, gather_cols = vars)))
    evaldt(x[, .variable := as.factor(.variable)])
    if (length(byvars)){
      print(x)
    print(ggplot(x, aes_string(y = value, x = group , weight = w)) + geom_boxplot(outlier.colour = NULL, aes_string(colour = group, fill = group))+  stat_summary(geom = "crossbar", width=0.65, fatten=0, fill = "white", aes_string(colour = group), fun.data =  mean_cl_boot, alpha = 0.3)  + stat_summary(geom = "crossbar", width=0.65, fatten=0, aes_string(color = group), fun.data =  function(x){m <- mean(x); c(ymin = m, ymax = m, y = m)}, alpha = 0.7) + facet_wrap(facets = as.formula(paste0("~",variable)), scales = "free") + stat_summary(geom = "crossbar", width=0.65, fatten=0, color = "white", fun.data =  function(x){m <- median(x, na.rm = TRUE); c(ymin = m, ymax = m, y = m)}, alpha = 0.7))
    } else{
      print(ggplot(x, aes_string(y = value, x = group , weight = w)) + geom_boxplot(outlier.colour = NULL, colour = hcl(h=15,l=65,c=100), fill = hcl(h=15,l=65,c=100))+  stat_summary(geom = "crossbar", width=0.65, fatten=0, color = hcl(h=15,l=65,c=100), fill = "white", fun.data =  mean_cl_boot, alpha = 0.3)+ stat_summary(geom = "crossbar", width=0.65, fatten=0, color = hcl(h=15,l=65,c=100), fun.data =  function(x){m <- mean(x); c(ymin = m, ymax = m, y = m)}, alpha = 0.7)  + facet_wrap(facets = as.formula(paste0("~",variable)), scales = "free")  +stat_summary(geom = "crossbar", width=0.65, fatten=0, color = "white", fun.data =  function(x){m <- median(x, na.rm = TRUE); c(ymin = m, ymax = m, y = m)}, alpha = 0.7))
    }
  } else{
    if (!length(byvars)){
      invisible(x[, describe_matrix(.SD,d = d, w = w, na.rm = na.rm ), .SDcols = vars])
    } else{
      invisible(x[, describe_matrix(.SD,d = d, w = w, na.rm = na.rm ), .SDcols = vars, by = byvars])
    }
  }
}



describe_matrix <- function(M, details = FALSE, na.rm = TRUE, w = NULL, mc.cores=getOption("mc.cores", 2L)){
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
    if (!is.null(w)){
      sum_mean <-as.data.frame(mclapply(M ,function(x){a <- sum(is.na(x)) ; c(length(x)-a,a, Hmisc::wtd.mean(x,na.rm=na.rm, w = w), sqrt(Hmisc::wtd.var(x,na.rm= na.rm)), Hmisc::wtd.quantile(x, c(0, 0.5, 1), na.rm = na.rm, weights = w))}))
    }else{
      sum_mean <-as.data.frame(mclapply(M ,function(x){a <- sum(is.na(x)) ; c(length(x)-a,a, mean(x,na.rm=na.rm, w = w), sd(x,na.rm= na.rm), quantile(x, c(0, 0.5, 1), type = 1, na.rm = na.rm, weights = w))}))
    }
    sum <- as.matrix(sum_mean)
    rownames(sum) <-  c("N","NA","Mean","Sd","Min", "50%", "Max")

  } else {
    N <- nrow(M)
    f=function(x){
      if (!is.null(w)){
        m <- Hmisc::wtd.mean(x, na.rm = na.rm, w = w)
        sum_higher <- matrixStats::colWeightedMeans(cbind((x-m)^2,(x-m)^3,(x-m)^4), na.rm=na.rm, w = w)
        sum_higher[1] <- sqrt(sum_higher[1])
        sum_higher[2] <- sum_higher[2]/sum_higher[1]^3
        sum_higher[3] <- sum_higher[3]/sum_higher[1]^4
        sum_quantile=Hmisc::wtd.quantile(x, c(0,0.01,0.05,0.1,0.25,0.50,0.75,0.9,0.95,0.99,1), na.rm=na.rm, weights = w)
      } else{
        m <-mean(x, na.rm = na.rm, w = w)
        sum_higher <- colMeans(cbind((x-m)^2,(x-m)^3,(x-m)^4), na.rm=na.rm, w = w)
        sum_higher[1] <- sqrt(sum_higher[1])
        sum_higher[2] <- sum_higher[2]/sum_higher[1]^3
        sum_higher[3] <- sum_higher[3]/sum_higher[1]^4
        sum_quantile= quantile(x, c(0,0.01,0.05,0.1,0.25,0.50,0.75,0.9,0.95,0.99,1),type= 1, na.rm=na.rm, weights = w)
      }
      n_NA <- sum(is.na(x))
      sum <- c(N-n_NA,n_NA,m,sum_higher,sum_quantile)
    }
    sum <- as.data.frame(mclapply(M, f))
    rownames(sum) <-  c("N","NA","Mean","Sd","Skewness","Kurtosis","Min","1%","5%","10%","25%","50%","75%","90%","95%","99%","Max")
    # rownames(sum) <- c("Rows","N","Mean","Sd","Skewness","Kurtosis","Min","1%","5%","10%","25%","50%","75%","90%","95%","99%","Max")
  }
  print <- apply(sum,c(1,2),
    function(x){
    if (is.numeric(x)){
      y <- .iround(x,decimal.places=3)
      y <- str_replace(y,"000$","")
      if (y==""){
        y <- "0"
      }
      y <- str_replace(y,"\\.$","")
      y <- str_replace(y,"-0","0")
    } else{
      y <- x
    }
    y
  })
  print(noquote(format(print,justify="right")),right=TRUE)
  cat("\n")
}


