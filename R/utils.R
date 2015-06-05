dt_env <- function(dt, env, byvars) {
  env <- new.env(parent = env, size = 2L)
  env$dt <- dt
  env$byvars <- byvars
  env
}


and_expr = function (exprs) 
{
    if (length(exprs) == 0) 
        return(TRUE)
    if (length(exprs) == 1) 
        return(exprs[[1]])
    left <- exprs[[1]]
    for (i in 2:length(exprs)) {
        left <- substitute(left & right, list(left = left, right = exprs[[i]]))
    }
    left
}


or_expr = function (exprs) 
{
    if (length(exprs) == 0) 
        return(TRUE)
    if (length(exprs) == 1) 
        return(exprs[[1]])
    left <- exprs[[1]]
    for (i in 2:length(exprs)) {
        left <- substitute(left | right, list(left = left, right = exprs[[i]]))
    }
    left
}


deparse_all <- function(x) {
  deparse2 <- function(x) paste(deparse(x, width.cutoff = 500L), collapse = "")
  vapply(x, deparse2, FUN.VALUE = character(1))
}


shallow <- function(x,...){
  shallow_(x = x, vars = lazyeval::lazy_dots(...))
}

shallow_ <- function(x, vars) {
    vars <- names(select_vars_(names(x), vars))
    if (length(vars) == 0) {
       vars <- names(x)
    }
    out = as.list(x)[vars]
    setDT(out)
    out
}

# from ggplot2
try_require <- function(package) {
  available <- suppressMessages(suppressWarnings(
    require(package, character.only = TRUE)
  ))

  if (!available) {
    stop(package, " package required for this functionality. " ,
      "Please install and try again.", call. = FALSE)
  }
}



assign_var <- function(x, ..., env = parent.frame(), inherits = FALSE){
    names <- sapply(lazy_dots(...), function(x){as.character(x$expr)})
    assign_var_(x = x, names = names, inherits = inherits, env = parent.frame())
}
assign_var_ <- function(x, names, env = parent.frame(), inherits=TRUE) {
    for (name in names){
        tempname <- tempname(paste("temp",name,sep="_"), where = env, inherits = inherits)
        assign(name, tempname, env)
    }
}


setmutate <- function(x, ..., i = NULL, by = NULL){
    setmutate_(x, vars = lazy_dots(...), i = substitute(i), by = substitute(by))
}

setmutate_ <- function(x, vars, i = NULL, by = NULL){
    stopifnot(is.data.table(x))
    byvars <- names(select_vars_(names(x), by))
    if (!length(by)){
        byvars <- NULL
    }
    if (!is.null(i)){
      i <- as.lazy(i)$expr
    }
    dots <- lazyeval::all_dots(vars, all_named = TRUE)
    env <- dt_env(x, lazyeval::common_env(dots), byvars)
     # For each new variable, generate a call of the form df[, new := expr]
     for(col in names(dots)) {
      if (is.null(byvars) & is.null(i)){
         call <- substitute(dt[, lhs := rhs],
           list(lhs = as.name(col), rhs = dots[[col]]$expr))
       } else if (is.null(byvars) & !is.null(i)){
        call <- substitute(dt[i, lhs := rhs],
          list(lhs = as.name(col), rhs = dots[[col]]$expr, i = i))
      } else if (!is.null(byvars) & is.null(i)){
        call <- substitute(dt[, lhs := rhs, by = c(byvars)],
          list(lhs = as.name(col), rhs = dots[[col]]$expr))
      } else {
        call <- substitute(dt[i, lhs := rhs, by = c(byvars)],
          list(lhs = as.name(col), rhs = dots[[col]]$expr, i = i))
      }
    eval(call, env)
    }
  x[]
}

setmutate_each <- function(x, funs, ..., i = NULL, by = NULL, replace = FALSE){
    setmutate_each_(x, funs, vars = lazy_dots(...), i = substitute(i), by = substitute(by), replace = replace)
}

setmutate_each_ <- function(x, funs, vars, i = NULL, by = NULL, replace = FALSE){
  stopifnot(is.data.table(x))
    if (anyDuplicated(names(funs))){
      stop("Multiple functions are specified with the same name", call. = FALSE)
    }
    if (replace & length(funs)!=1){
      stop("replace is TRUE but not one function is specified", call. = FALSE)
    }
    byvars <- names(select_vars_(names(x), by))
    if (!length(by)){
        byvars <- NULL
    }
    vars <- lazyeval::all_dots(vars)
    vars <- colwise_(x, funs_(funs), vars, byvars = byvars, replace = replace)
    setmutate_(x, vars, i, by)
}



colwise_ <- function(tbl, calls, vars, byvars = NULL, replace = FALSE) {
  vars <- select_vars_(tbl_vars(tbl), vars, exclude = byvars)
  if (!length(vars)){
    vars <- setdiff(names(tbl), byvars)
  }
  out <- vector("list", length(vars) * length(calls))
  dim(out) <- c(length(vars), length(calls))
  for (i in seq_along(vars)) {
    for (j in seq_along(calls)) {
      out[[i, j]] <- lazyeval::interp(calls[[j]],
        .values = list(. = as.name(vars[i])))
    }
  }
  dim(out) <- NULL  
  # modification is here:
  if (length(calls) == 1 & replace) {
    names(out) <- names(vars)
  } else {
    grid <- expand.grid(var = names(vars), call = names(calls))
    names(out) <- paste(grid$var, grid$call, sep = "_")
  }
  out
}





print_all <- function(x){
  print(x, nrow(x))
}


##' Experimental function to graph a dataset
##' 
##' @param x A data.frame
##' @param formula A formula.
##' @param w Wweights 
##' @param n Number of quantiles to plot
##' @param A plot of the lhs of the formula over the first regressor, after controlling by #variables in the formula.
##' @examples
##' library(dplyr)
##' binscatter(iris, Sepal.Width ~ Sepal.Length)
##' binscatter(iris, Sepal.Width ~ Sepal.Length | Species, n = 10)
##' binscatter(group_by(iris, Species), Sepal.Width ~ Sepal.Length, n = 10)
##' @export
#binscatter <- function(x, formula, w = NULL, verbose = FALSE, n = 20) {
#  binscatter_(x, formula, w = substitute(w), verbose = verbose, n = n)
#}
#
##' @export
##' @rdname binscatter
#binscatter_<- function(x, formula, w = NULL, verbose = FALSE, n = 20){
#  bin <- "bin"
#  intercept <- "intercept"
#  slope <- "slope"
#  ww <- "ww"
#  w <- names(select_vars_(names(x),w))
#  byvars <-  vapply(groups(x), as.character, character(1))
#  if (length(byvars)>1){
#    group <- tempname(x, 1)
#    x <- mutate_(x, .dots = setNames(list(~as.factor(group_indices_(x, .dots = byvars))), group)#)
#  } else if (length(byvars)==1){
#    group <- byvars
#  } else{
#    group <- character(0)
#  }
#  if (!length(w)){
#    x <- mutate_(x, .dots = setNames(list(~1), ww))
#  } else{
#   x <-  mutate_(x, .dots = setNames(list(interp(~w/sum(w), w = as.name(w))), ww))
#  }
#  x <- select_(x, .dots = c(all.vars(formula), ww))
#  x <- na.omit(x) 
#  F <- Formula(formula)
#  F1 <- formula(F, lhs = 1, rhs = 1)
#  F2 <- formula(F, lhs = 0, rhs = - 1)
#  y <- deparse(F1[[2]])
#  x1 <- deparse(F1[[3]])
#  newformula <- as.Formula(~1, F2)
#  ans <- do_(x, .dots = interp(~f(y, x1, newformula, ww, .), y = y, x1 = x1, newformula = #newformula, ww = ww))
#  coeff <- do_(group_by_(ans, .dots = group), .dots = interp(~data.frame(t(coef(lm(formula,  #data = . , weights = ww)))), formula = as.formula(paste0(y, "~", x1)), ww = as.name(ww)))
#  coeff <- setNames(coeff, c(group, intercept, slope))
#  ans <- mutate_(ans, .dots = setNames(list(interp(~xtile(x1, n, ww), x1 = as.name(x1), ww = as.#name(ww))), bin))
#  ans <- group_by_(ans, .dots = bin, add = TRUE)
#  summary <- summarize_(ans, .dots = setNames(list(interp(~mean(x1), x1 = as.name(x1)),interp(~#mean(y), y = as.name(y))), c(x1, y)))
#  if (length(group)){
#    coeff <- ungroup(coeff)
#    coeff <- mutate_(coeff, .dots = setNames(list(interp(~as.factor(group), group = as.name(#group))), group))
#    summary <- ungroup(summary)
#    summary <- mutate_(summary, .dots = setNames( list(interp(~as.factor(group), group = as.name#(group))), group))
#    g <-  ggplot(summary, aes_string(x = x1, y = y, color = group)) + geom_point() + geom_abline#(data = coeff, aes_string(intercept = intercept, slope = slope, color = group))
#  } else{
#    g <-  ggplot(summary, aes_string(x = x1, y = y)) + geom_point(colour = hcl(h=15,l=65,c=100))# + geom_abline(data = coeff, aes_string(intercept = intercept, slope = slope))
#  }
#  print(g)
#}
#
#
#
#f <- function(varname1, varname2, formula, ww, df){
#  felm1 <- felm(as.formula(paste0(varname1, deparse(formula))), df, weights = df[[ww]])
#  out1 <- mean(df[[varname1]]) + felm1$residuals
#  felm2 <- felm(as.formula(paste0(varname2, deparse(formula))), df, weights = df[[ww]])
#  out2 <- mean(df[[varname2]]) + felm2$residuals
#  setNames(data.frame(out1, out2, df[[ww]]), c(varname1, varname2, ww))
#}




##' Experimental function to graph a dataset
##' 
##' @param x A data.table.
##' @param ... Variables to include. Defaults to all non-grouping variables. See the \link[dplyr]#{select} documentation.
##' @param by Groups within which variables should be ploted.
##' @param type type of graph among "density", "boxplot", "line", or a statistical method (like #"lm", "loeless" or "felm")
##' @param along_with A variable that specifies the x axis. Should be specified when "type" is #"line", "lm" or "loeless".
##' @param winsorize Should variables winsorized?
##' @param reorder Should the category with the most count be printed first?
##' @param facet Should different groups graphed in different windows?
##' @param verbose Should warnings (regarding missing values, outliers, etc) be printed?
##' @param .dots Used to work around non-standard evaluation.
##' @param w Analytical weights (experimental)
##' @param n Number of quantiles to plot
##' @examples
##' library(data.table)
##' N <- 1e2
##' DT <- data.table(
##'   id = sample(c("id1","id2","id3"), N, TRUE),
##'   v1 = sample(c(1:5), N, TRUE),
##'   v2 = rnorm(N, sd = 20),
##'   v3 = sample(runif(100, max=100), N, TRUE)
##' )
##' DT[, v4 := v3 + rnorm(N, sd = 20)]
##' graph(DT)
##' graph(DT, by = id)
##' graph(DT, by = id, facet = TRUE)
##' graph(DT, by = id, type = "boxplot")
##' graph(DT, v3, along_with = v2, by = id, type = "loess")
##' @export
#graph <- function(x, ..., along_with = NULL, by = NULL, w = NULL, reorder = TRUE, winsorize = #TRUE, facet = FALSE, verbose = FALSE, type = if (is.null(substitute(along_with))){"density"} #else {"loess"}, n = 20) {
#  graph_(x, .dots = lazy_dots(...) , along_with = substitute(along_with), by = substitute(by), #w = substitute(w), reorder = reorder, winsorize = winsorize, facet = facet, verbose = #verbose, type = type, n = n)
#}
#
##' @export
##' @rdname graph
#graph_<- function(x, ..., .dots , along_with = NULL, by = NULL, w = NULL, reorder = TRUE, #winsorize = TRUE , facet = FALSE, verbose = FALSE, type = if (is.null(along_with)){ "density"}# else {"loess" }, n = 20){
#  stopifnot(is.data.table(x))
#  w <- names(select_vars_(names(x),w))
#  along_with <- names(select_vars_(names(x), along_with))
#  byvars <- names(select_vars_(names(x), by))
#  dots <- all_dots(.dots, ...)
#  vars <- names(select_vars_(names(x), dots, exclude = c(byvars,w,along_with)))
#  if (length(vars) == 0) {
#     vars <- setdiff(names(x), c(byvars, along_with, w))
#  }
#  if (length(along_with) | type == "boxplot"){
#    nums <- sapply(x, is.numeric)
#    nums_name <- names(nums[nums==TRUE])
#    vars = intersect(vars,nums_name)
#  }
#  if (!length(vars)) stop("Please select at least one non-numeric variable", call. = FALSE)
#
#  tempname <- tempname(x, n = 7)
#  group <- tempname[1]
#  count <- tempname[2]
#  variable <- tempname[3]
#  value <- tempname[4]
#  bin <- tempname[5]
#  intercept <- tempname[6]
#  slope <- tempname[7]
#  x <- shallow_(x, c(byvars, vars, along_with, w))
#  if (!length(w)){
#    w <- tempname(x)
#    x[, (w) := 1]
#    ww <- NULL
#  } else{
#    ww <- as.name(paste0(w,"/sum(",w,")"))
#  }
#  if (winsorize){
#    v <-  c(vars, along_with, w)
#    if (length(along_with)){
#      nums <- sapply(x, is.numeric)
#    } else{
#      nums <- sapply(x, is.double)
#    }
#    nums_name <- names(nums[nums==TRUE])
#    v = intersect(v,nums_name)
#    x[, (v) := lapply(.SD,function(x){winsorize(x, verbose = verbose)}), .SDcols = c(v)]
#  }
#  if (length(byvars)>1){
#    x[, (group):= .GRP, by = c(byvars)]
#    x[, (group) := as.factor(get(group))]
#  } else if (length(byvars)==1){
#    group <- byvars
#  } else{
#    group <- character(0)
#  }
#  # type boxplot
#  if (type == "boxplot"){
#    old = theme_get()
#    on.exit(theme_set(old))
#    theme = theme_set(theme_minimal())
#    theme = theme_update(legend.position="top", legend.title=element_blank(), panel.grid.major.x#=element_blank())
#    theme = theme_update(axis.text.x=element_blank(), axis.ticks.x = element_blank(), axis.line.#x = element_blank(), axis.title.x=element_blank())
#    theme = theme_update(axis.line.y = element_blank(), axis.title.y=element_blank(), axis.text.#y = element_text(colour="grey"), axis.ticks.y= element_line(colour="grey"))
#    if (!length(w)){
#      ww <- NULL
#    }
#    x <-  suppressWarnings(suppressMessages(gather_(x, variable, value, vars)))
#    x[, (variable) := as.factor(get(variable))]
#    if (length(byvars)){
#      print(ggplot(x, aes_string(y = value, x = group , weight = ww)) + geom_boxplot(outlier.#colour = NULL, outlier.size = 1, notch = TRUE,  aes_string(colour = group, fill = group)#)+  stat_summary(geom = "crossbar", width=0.65, fatten=0, fill = "grey", color = "grey"#, fun.data =  mean_cl_boot, alpha = 0.5)  + facet_wrap(facets = as.formula(paste0("~",#variable)), scales = "free") + expand_limits(y = 0)) #+ stat_summary(geom = "crossbar", #width=0.65, fatten=0, color = "white", fun.data =  function(x){m <- median(x, na.rm = #TRUE); c(ymin = m, ymax = m, y = m)}, alpha = 0.7))
#    } else{
#      print(ggplot(x, aes_string(y = value, x = group , weight = ww)) + geom_boxplot(outlier.#colour = NULL, outlier.size = 1, notch = TRUE, colour = hcl(h=15,l=65,c=100), fill = hcl#(h=15,l=65,c=100), width = 0.5)+  stat_summary(geom = "crossbar", width=0.65/2, fatten=0#, color = "grey", fill = "grey", fun.data =  mean_cl_boot, alpha = 0.5) + facet_wrap(#facets = as.formula(paste0("~",variable)), scales = "free") + expand_limits(y = 0))  ##+stat_summary(geom = "crossbar", width=0.65, fatten=0, color = "white", fun.data =  #function(x){m <- median(x, na.rm = TRUE); c(ymin = m, ymax = m, y = m)}, alpha = 0.7))
#    }
#  } else{
#  # other type
#    g <- NULL
#    i <- 0
#      for (v in vars){
#        ans <- shallow_(x, c(group, v, w, along_with))
#        i <- i+1
#        if (length(along_with)){
#        # along_with
#          ans <- na.omit(ans)
#          if (type == "line"){
#            # line
#            if (!facet){
#              g[[i]] <-  ggplot(ans, aes_string(weight = ww, x = along_with, y = v, colour = #group)) + geom_line() 
#            } else{
#              g[[i]] <-  ggplot(ans, aes_string(weight = ww, x = along_with, y = v)) + #facet_grid(as.formula(paste0(group, "~.")))
#            }
#          } else{
#            # if no formula, use stat.smooth
#            ans[, (bin) := xtile(get(along_with), n = n, w = get(w))]
#            N <- ans[, sum(get(w))]
#            ans2 <- shallow(ans)
#            ans2[, c(along_with,v) := list(weighted.mean(get(along_with), get(w)), weighted.mean#(get(v),  get(w))), by = c(group, bin)]
#            if (!facet){
#                if (length(group)){
#                   ans[, (group):= as.factor(get(group))]
#                   ans2[, (group):= as.factor(get(group))]
#                 } else{
#                  group <- NULL
#                }
#                 g[[i]] <-  ggplot(ans, aes_string(weight = ww, x = along_with, y = v, color = #group)) + geom_point(data = ans2, aes_string(x = along_with, y = v, color = #group), alpha = 0.6) + stat_smooth(method = type)
#            } else{
#                 g[[i]] <-  ggplot(ans, aes_string(weight = ww, x = along_with, y = v)) + #geom_point(data = ans2, aes_string(weight = ww, x = along_with, y = v)) + #stat_smooth(method = type) + facet_grid(as.formula(paste0(group, "~.")))
#            } 
#          } 
#        } else{
#        # no along with
#          dummy <- is.integer(ans[,get(v)])+ is.character(ans[,get(v)])
#          if (dummy) {
#            # case of integer: bars
#            setkeyv(ans, c(v, group))
#            ans[, (v) := as.factor(get(v))]
#            if (!facet){
#              if (length(group)){
#                ans[, (group):= as.factor(get(group))]
#              } else{
#                group <- NULL
#              }
#              g[[i]] <-  ggplot(ans, aes_string(weight = ww, x = v, fill = group)) + geom_bar(#width = 0.5, position = "dodge")+ coord_flip() + expand_limits(y=0)
#            } else{
#                g[[i]] <-  ggplot(ans, aes_string(weight = ww, x = v)) + geom_point(stat="bin") #+ coord_flip() + expand_limits(y = 0)+ facet_grid(as.formula(paste0(group,"~."#)))
#            }
#          } else{ 
#            # case of continuous: density
#            if (!facet){
#              if (length(group)){
#                ans[, (group):= as.factor(get(group))]
#              }
#              g[[i]] <-  ggplot(ans, aes_string(weight = ww, x = v, color = group)) + #stat_density(geom = "line", position = "identity") + expand_limits(y=0)
#            } else{            
#            g[[i]] <-  ggplot(ans, aes_string(weight = ww, x = v)) + stat_density(geom = "line")# + facet_grid(as.formula(paste0(group, "~."))) + expand_limits(y=0)
#            }
#          }
#        
#      } 
#  }
#    if (length(g)==1){
#      if (verbose){
#        print(g[[1]])
#      } else{
#        suppressWarnings(suppressMessages(print(g[[1]])))
#      }
#    } else{
#      if (verbose){
#        do.call(multiplot, g)
#      }
#      suppressWarnings(suppressMessages(do.call(multiplot, g)))
#    }
#  }
#}
#   
#
#
#
## from http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)
#multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
#  # Make a list from the ... arguments and plotlist
#  plots <- c(list(...), plotlist)
#  numPlots = length(plots)
#  # If layout is NULL, then use 'cols' to determine layout
#  if (is.null(layout)) {
#    # Make the panel
#    # ncol: Number of columns of plots
#    # nrow: Number of rows needed, calculated from # of cols
#    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
#                    ncol = cols, nrow = ceiling(numPlots/cols))
#  }
# if (numPlots==1) {
#    print(plots[[1]])
#
#  } else {
#    # Set up the page
#    grid.newpage()
#    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
#
#    # Make each plot, in the correct location
#    for (i in 1:numPlots) {
#      # Get the i,j matrix positions of the regions that contain this subplot
#      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
#      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
#                                      layout.pos.col = matchidx$col))
#    }
#  }
#}


##' Demean a vector 
##' 
##' @param x A vector, a list of vector, or a data.frame
##' @param fe List of vectors for group (factor, characters or integers)
##' @return A demeaned vector
##' @details This function calls felm::demeanlist after dealing with missing values and #converting group variables into factors
##' @return An object of the same type than `x` (ie vector, list or data.frame) where each vector# is replaced by its demaned version.
##' @examples       
##' #demean(c(1,2), fe = c(1L,1L))  
##' #demean(c(NA,2), fe = list(c(1L,2L), c(1L,3L)))               
##' #demean(c(1,2), fe = list(c(NA,2L), c(1L,3L)))
##' #demean(list(c(1,2),c(1,4)), fe = list(c(NA,2L), c(1L,3L)))
##' @export
#demean <- function(x, fe){
#  flag <- ""
#  if (is.atomic(x)){
#    flag <- "atomic"
#  } else if (is.data.frame(x)){
#    flag <- "data.frame"
#  } 
#  x <- as.data.frame(x)
#  x_c <- names(x)[!sapply(x, is.double)]
#  if (length(x_c)){
#    x <- mutate_each_(x, funs(as.double), vars = x_c)
#  }
#  if (is.atomic(fe)){
#    fe <- list(fe)
#  }
#  fe <- as.data.frame(fe)
#  fe_c <- names(fe)[!sapply(fe, is.factor)]
#  if (length(fe_c)){
#    fe <- mutate_each_(fe, funs(as.factor), vars = fe_c)
#  }
#  nrow <- nrow(x)
#  rows <- complete.cases(x) & complete.cases(fe)
#  m <- demeanlist(filter(x, rows), filter(fe, rows))
#  out <- lapply(m, function(y){
#    out <- rep(NA, nrow)
#    out[rows] <- y
#    attributes(out) <- NULL
#    out
#  })
#  names(out) <- NULL
#  if (flag == "atomic"){
#    out <- out[[1]]
#  } else if (flag == "data.frame"){
#    out <- as.data.frame(out)
#  }
#  out
#}









