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



##' Keep only certain columns in place 
##'
##' @param x a data.table 
##' @param ... Variables to keep. Default to all. See the \link[dplyr]{select} documentation.
##' @param vars Used to work around non-standard evaluation.
##' @examples
##' library(data.table)
##' DT <- data.table(
##'   id = c(1,2),
##'   v1 = c(1,1),
##'   v2 = c(2,1)
##' )
##' setkeep(DT, id, v2)
##' setkeep(DT, -id)
##' @export
#setkeep <- function(x, ...){
#  setkeep_(x = x, vars = lazyeval::lazy_dots(...))
#}
#
##' @export
##' @rdname setkeep
#setkeep_ <- function(x, vars){
#  stopifnot(is.data.table(x))
#  dots <- lazyeval::all_dots(vars)
#  vars <- names(select_vars_(names(x), dots))
#  if (!length(vars))  vars <- names(x)
#  discard <- setdiff(names(x), vars)
#  if (length(discard)>0){
#    x[, c(discard) := NULL]
#  }
#}
#
#
##' discard certain columns in place 
##'
##' @param x a data.table 
##' @param ... Variables to keep. Default to all. See the \link[dplyr]{select} documentation.
##' @param vars Used to work around non-standard evaluation.
##' @examples
##' library(data.table)
##' DT <- data.table(
##'   id = c(1,2),
##'   v1 = c(1,1),
##'   v2 = c(2,1)
##' )
##' setdiscard(DT, id)
##' @export
#setdiscard <- function(x, ...){
#  setdiscard_(x = x, vars = lazyeval::lazy_dots(...))
#}
##' @export
##' @rdname setdiscard
#setdiscard_ <- function(x, vars){
#  stopifnot(is.data.table(x))
#  dots <- lazyeval::all_dots(vars)
#  vars <- names(select_vars_(names(x), dots))
#  if (!length(vars))  vars <- names(x)
#  if (length(discard)>0){
#    x[, c(vars) := NULL]
#  }
#}
#
#
##' Create new data.table by keeping only certain columns (equivalent to dplyr::select)
##'
##' @param x a data.table 
##' @param ... Variables to keep. Default to all. See the \link[dplyr]{select} documentation.
##' @param vars Used to work around non-standard evaluation.
##' @examples
##' library(data.table)
##' DT <- data.table(
##'   id = c(1,2),
##'   v1 = c(1,1),
##'   v2 = c(2,1)
##' )
##' keep(DT, id, v2)
##' keep(DT, -id)
##' @export
#keep <- function(x, ...){
#  keep_(x = x, vars = lazyeval::lazy_dots(...))
#}
##' @export
##' @rdname keep
#keep_ <- function(x, vars){
#  stopifnot(is.data.table(x))
#  dots <-  lazyeval::all_dots(vars)
#  vars <- names(select_vars_(names(x), dots))
#  if (!length(vars))  vars <- names(x)
#  x[, vars, with = FALSE]
#}
#
#
##' Create a new data.table by discarding certain columns 
##'
##' @param x a data.table 
##' @param ... Variables to keep. Default to all. See the \link[dplyr]{select} documentation.
##' @param vars Used to work around non-standard evaluation.
##' @examples
##' library(data.table)
##' DT <- data.table(
##'   id = c(1,2),
##'   v1 = c(1,1),
##'   v2 = c(2,1)
##' )
##' discard(DT, id, v2)
##' discard(DT, -id)
##' @export
#discard <- function(x, ...){
#  discard_(x = x, vars = lazyeval::lazy_dots(...))
#}
#
##' @export
##' @rdname discard
#discard_ <- function(x, vars){
#  stopifnot(is.data.table(x))
#  dots <-  lazyeval::all_dots(vars)
#  vars <- names(select_vars_(names(x), dots))
#  keep <- setdiff(names(x), vars)
#  x[, keep, with = FALSE]
#}
#
#
#
##' Create new data.table by keeping only certain rows(equivalent to dplyr::filter)
##'
##' @param x a data.table 
##' @param ... Conditions
##' @param by groups in which the condition should be evaluated
##' @param .dots Used to work around non-standard evaluation.
##' @examples
##' library(data.table)
##' DT <- data.table(
##'   id = c(1,2,1),
##'   v1 = c(1,NA,2)
##' )
##' keep_if(DT, v1 == 1)
##' keep_if(DT, v1 == min(v1), by = id)
##' @export
#keep_if <- function(x, ..., by = NULL){
#  keep_if_(x = x, .dots = lazyeval::lazy_dots(...), by = substitute(by))
#}
#
##' @export
##' @rdname keep_if
#keep_if_ <- function(x, .dots, by = NULL){
#  stopifnot(is.data.table(x))
#  byvars <- names(select_vars_(names(x), by))
#  if (!length(by)){
#      byvars <- NULL
#  }
#  dots <-  lazyeval::all_dots(.dots)
#  expr <- lapply(dots, `[[`, "expr")
#  call <- substitute(dt[, .I[expr], by = byvars], list(expr=and_expr(expr)))
#  env <- dt_env(x, lazyeval::common_env(dots), byvars = byvars)
#  ans <- eval(call, env)
#  indices <- ans[[length(ans)]]
#  x[indices[!is.na(indices)]]
#}
#
#
#
##' Create new data.table after discarding certain rows
##'
##' @param x a data.table 
##' @param ... Conditions. Rows where the condition evaluates to NA are not discardd. Therefore, \code{discard_if(dt, condition)} is #not the same as \code{keep_if(x, !condition)} with 
##' @param by groups in which the condition should be evaluated
##' @param .dots Used to work around non-standard evaluation.
##' @examples
##' library(data.table)
##' DT <- data.table(
##'   id = c(1,2,1),
##'   v1 = c(1,NA,2)
##' )
##' discard_if(DT, v1 == 1)
##' discard_if(DT, v1 == 1, v1 == 2)
##' discard_if(DT, v1 == min(v1), by = id)
##' @export
#discard_if <- function(x, ..., by = NULL){
#  discard_if_(x = x, .dots = lazyeval::lazy_dots(...), by = substitute(by))
#}
#
##' @export
##' @rdname discard_if
#discard_if_ <- function(x, .dots, by = NULL){
#  stopifnot(is.data.table(x))
#  byvars <- names(select_vars_(names(x), by))
#  if (!length(by)){
#      byvars <- NULL
#  }
#  dots <-  lazyeval::all_dots(.dots)
#  expr <- lapply(dots, `[[`, "expr")
#  call <- substitute(dt[, .I[expr], by = byvars], list(expr=or_expr(expr)))
#  env <- dt_env(x, lazyeval::common_env(dots), byvars = byvars)
#  ans <- eval(call, env)
#  indices <- ans[[length(ans)]]
#  if (!length(indices)){
#    out <- x
#  } else{
#    out <- x[-indices[!is.na(indices)]]
#  }
#  out
#}


##' fill NA in place based on non missing observations
##'
##' @param x a data.table 
##' @param ... Variables to fill in. Default to all non grouping variables. See the \link[dplyr]{select} documentation.
##' @param along_with Numeric variable along which NAs should be filled. Default to last key. See the \link[dplyr]{select} #documentation.
##' @param by Groups within which gaps should be fill. Default to keys (or to keys minus last if along_with is unspecified). See the \#link[dplyr]{select} documentation.
##' @param roll When roll is a positive number, this limits how far values are carried forward. roll=TRUE is equivalent to roll=+Inf. #When roll is a negative number, values are rolled backwards; i.e., next observation carried backwards (NOCB). Use -Inf for unlimited #roll back. When roll is "nearest", the nearest value is joined to.
##' @param rollends  A logical vector length 2 (a single logical is recycled). When rolling forward (e.g. roll=TRUE) if a value is past# the last observation within each group defined by the join columns, rollends[2]=TRUE will roll the last value forwards. rollends[1]#=TRUE will roll the first value backwards if the value is before it. If rollends=FALSE the value of i must fall in a gap in x but #not after the end or before the beginning of the data, for that group defined by all but the last join column. When roll is a finite #number, that limit is also applied when rolling the end
##' @param vars Used to work around non-standard evaluation.
##' @examples
##' library(data.table)
##' DT <- data.table(
##'  id    = c(1, 1, 1, 1, 2, 2),
##'  date  = c(1992, 1989, 1991, 1993, 1992, 1991),
##'  value = c(NA, NA, 3, NA, 3.2, 5.2)
##' )
##' DT1 <- copy(DT)
##' setkey(DT1, id, date)
##' DT2 <- copy(DT1)
##' DT3 <- copy(DT1)
##' setna(DT, value, along_with = date, by = id)
##' setna(DT1)
##' setna(DT2, value, rollends = TRUE)
##' setna(DT3, value, roll = "nearest")
##' @export
#setna <- function(x, ..., along_with = NULL, by = NULL, roll = TRUE,  rollends = if (roll=="nearest") c(TRUE,TRUE)
#  else if (roll>=0) c(FALSE,TRUE)
#  else c(TRUE,FALSE)){
#    setna_(x, vars = lazy_dots(...) , along_with = substitute(along_with),  by = substitute(by), roll = roll, rollends = rollends)
#}
#
##' @export
##' @rdname setna
#setna_ <- function(x, ..., vars, along_with = NULL, by = NULL, roll = TRUE,  rollends = if (roll=="nearest") c(TRUE,TRUE)
#  else if (roll>=0) c(FALSE,TRUE)
#  else c(TRUE,FALSE)){
#  stopifnot(is.data.table(x))
#  byvars <- names(select_vars_(names(x), by))
#  along_with  <- names(select_vars_(names(x), along_with))
#  if (!length(byvars) & (!length(along_with))){
#      byvars <- head(key(x),-1)
#      along_with <- tail(key(x),1)
#      if (!length(along_with)) stop("along_with is not specified but x is not keyed")
#  } else if (!length(byvars)){
#      byvars <- key(x)
#  } else if (!length(along_with)){
#      stop("When by is specified, along_with must also be specified")
#  }
#  setkeyv(x, c(byvars, along_with))
#  dots <- all_dots(vars)
#  vars <- names(select_vars_(names(x), dots, exclude = c(byvars, along_with)))  
#  if (length(vars) == 0) {
#     vars <- setdiff(names(x),c(byvars, along_with))
#  }
#  for (col in vars){
#    eval(substitute(x[, (col) := x[!is.na(t), c(byvars,along_with, col), with = FALSE ][x, value, roll = roll, rollends = rollends]], #list(t = as.name(col))))
#  }
#  x[]
#}






