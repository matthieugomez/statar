#' Experimental function to graph a dataset
#' 
#' @param x A data.table.
#' @param ... Variables to include. Defaults to all non-grouping variables. See the \link[dplyr]{select} documentation.
#' @param by Groups within which variables should be ploted.
#' @param type type of graph among "density", "boxplot", "line", or a statistical method (like "lm", "loeless" or "felm")
#' @param along_with A variable that specifies the x axis. Should be specified when "type" is "line", "lm" or "loeless".
#' @param winsorize Should variables winsorized?
#' @param reorder Should the category with the most count be printed first?
#' @param facet Should different groups graphed in different windows?
#' @param verbose Should warnings (regarding missing values, outliers, etc) be printed?
#' @param .dots Used to work around non-standard evaluation.
#' @param formula to use then type is "felm"
#' @param w Analytical weights (experimental)
#' @param n Number of quantiles to plot
#' @examples
#' library(data.table)
#' N <- 1e2
#' DT <- data.table(
#'   id = sample(c("id1","id2","id3"), N, TRUE),
#'   v1 = sample(c(1:5), N, TRUE),
#'   v2 = rnorm(N, sd = 20),
#'   v3 = sample(runif(100, max=100), N, TRUE)
#' )
#' DT[, v4 := v3 + rnorm(N, sd = 20)]
#' graph(DT)
#' graph(DT, by = id)
#' graph(DT, by = id, facet = TRUE)
#' graph(DT, by = id, type = "boxplot")
#' graph(DT, v3, along_with = v2, by = id, type = "loess")
#' @export
graph <- function(x, ..., along_with = NULL, by = NULL, w = NULL, reorder = TRUE, winsorize = TRUE, facet = FALSE, verbose = FALSE, type = if (is.null(substitute(along_with))){"density"} else {if (is.null(formula)) "loess" else "felm"}, formula = NULL, n = 20) {
  graph_(x, .dots = lazy_dots(...) , along_with = substitute(along_with), by = substitute(by), w = substitute(w), reorder = reorder, winsorize = winsorize, facet = facet, verbose = verbose, type = type, formula = formula, n = n)
}

#' @export
#' @rdname graph
graph_<- function(x, ..., .dots , along_with = NULL, by = NULL, w = NULL, reorder = TRUE, winsorize = TRUE , facet = FALSE, verbose = FALSE, type = if (is.null(along_with)){ "density"} else {if (is.null(formula)) "loess" else "felm"}, formula = NULL, n = 20){
  stopifnot(is.data.table(x))
  w <- names(select_vars_(names(x),w))
  along_with <- names(select_vars_(names(x), along_with))
  byvars <- names(select_vars_(names(x), by))
  dots <- all_dots(.dots, ...)
  vars <- names(select_vars_(names(x), dots, exclude = c(byvars,w,along_with)))
  if (length(vars) == 0) {
     vars <- setdiff(names(x), c(byvars, along_with, w))
  }
  if (length(along_with) | type == "boxplot"){
    nums <- sapply(x, is.numeric)
    nums_name <- names(nums[nums==TRUE])
    vars = intersect(vars,nums_name)
  }
  if (!length(vars)) stop("Please select at least one non-numeric variable", call. = FALSE)

  tempname <- tempname(x, n = 7)
  group <- tempname[1]
  count <- tempname[2]
  variable <- tempname[3]
  value <- tempname[4]
  bin <- tempname[5]
  intercept <- tempname[6]
  slope <- tempname[7]
  x <- shallow_(x, c(byvars, vars, along_with, w, all.vars(formula)))
  if (!length(w)){
    w <- tempname(x)
    x[, (w) := 1]
    ww <- NULL
  } else{
    ww <- as.name(paste0(w,"/sum(",w,")"))
  }
  if (winsorize){
    v <-  c(vars, along_with, w)
    if (length(along_with)){
      nums <- sapply(x, is.numeric)
    } else{
      nums <- sapply(x, is.double)
    }
    nums_name <- names(nums[nums==TRUE])
    v = intersect(v,nums_name)
    x[, (v) := lapply(.SD,function(x){winsorize(x, verbose = verbose)}), .SDcols = c(v)]
  }
  if (length(byvars)>1){
    x[, (group):= .GRP, by = c(byvars)]
    x[, (group) := as.factor(get(group))]
  } else if (length(byvars)==1){
    group <- byvars
  } else{
    group <- character(0)
  }
  # type boxplot
  if (type == "boxplot"){
    old = theme_get()
    on.exit(theme_set(old))
    theme = theme_set(theme_minimal())
    theme = theme_update(legend.position="top", legend.title=element_blank(), panel.grid.major.x=element_blank())
    theme = theme_update(axis.text.x=element_blank(), axis.ticks.x = element_blank(), axis.line.x = element_blank(), axis.title.x=element_blank())
    theme = theme_update(axis.line.y = element_blank(), axis.title.y=element_blank(), axis.text.y = element_text(colour="grey"), axis.ticks.y= element_line(colour="grey"))
    if (!length(w)){
      ww <- NULL
    }
    x <-  suppressWarnings(suppressMessages(gather_(x, variable, value, vars)))
    x[, (variable) := as.factor(get(variable))]
    if (length(byvars)){
      print(ggplot(x, aes_string(y = value, x = group , weight = ww)) + geom_boxplot(outlier.colour = NULL, outlier.size = 1, notch = TRUE,  aes_string(colour = group, fill = group))+  stat_summary(geom = "crossbar", width=0.65, fatten=0, fill = "grey", color = "grey", fun.data =  mean_cl_boot, alpha = 0.5)  + facet_wrap(facets = as.formula(paste0("~",variable)), scales = "free") + expand_limits(y = 0)) #+ stat_summary(geom = "crossbar", width=0.65, fatten=0, color = "white", fun.data =  function(x){m <- median(x, na.rm = TRUE); c(ymin = m, ymax = m, y = m)}, alpha = 0.7))
    } else{
      print(ggplot(x, aes_string(y = value, x = group , weight = ww)) + geom_boxplot(outlier.colour = NULL, outlier.size = 1, notch = TRUE, colour = hcl(h=15,l=65,c=100), fill = hcl(h=15,l=65,c=100), width = 0.5)+  stat_summary(geom = "crossbar", width=0.65/2, fatten=0, color = "grey", fill = "grey", fun.data =  mean_cl_boot, alpha = 0.5) + facet_wrap(facets = as.formula(paste0("~",variable)), scales = "free") + expand_limits(y = 0))  #+stat_summary(geom = "crossbar", width=0.65, fatten=0, color = "white", fun.data =  function(x){m <- median(x, na.rm = TRUE); c(ymin = m, ymax = m, y = m)}, alpha = 0.7))
    }
  } else{
  # other type
    g <- NULL
    i <- 0
      for (v in vars){
        ans <- shallow_(x, c(group, v, w, along_with, all.vars(formula)))
        i <- i+1
        if (length(along_with)){
        # along_with
          ans <- na.omit(ans)
          if (type == "line"){
            # line
            if (!facet){
              g[[i]] <-  ggplot(ans, aes_string(weight = ww, x = along_with, y = v, colour = group)) + geom_line() 
            } else{
              g[[i]] <-  ggplot(ans, aes_string(weight = ww, x = along_with, y = v)) + facet_grid(as.formula(paste0(group, "~.")))
            }
          } else if (is.null(formula)){
            # if no formula, use stat.smooth
            ans[, (bin) := xtile(get(along_with), n = n, w = get(w))]
            N <- ans[, sum(get(w))]
            ans2 <- shallow(ans)
            ans2[, c(along_with,v) := list(weighted.mean(get(along_with), get(w)), weighted.mean(get(v),  get(w))), by = c(group, bin)]
            if (!facet){
                if (length(group)){
                   ans[, (group):= as.factor(get(group))]
                   ans2[, (group):= as.factor(get(group))]
                 } else{
                  group <- NULL
                }
                 g[[i]] <-  ggplot(ans, aes_string(weight = ww, x = along_with, y = v, color = group)) + geom_point(data = ans2, aes_string(x = along_with, y = v, color = group), alpha = 0.6) + stat_smooth(method = type)
            } else{
                 g[[i]] <-  ggplot(ans, aes_string(weight = ww, x = along_with, y = v)) + geom_point(data = ans2, aes_string(weight = ww, x = along_with, y = v)) + stat_smooth(method = type) + facet_grid(as.formula(paste0(group, "~.")))
            } 
          } else {
            # statistical estimation
            f <- match.fun(type)
            ans[, c(v, along_with) := { 
              formula_v <- as.formula(paste0(v, deparse(formula)))
              out_v <- mean(.SD[[v]]) + residuals(f(formula_v, .SD))
              formula_along_with <- as.formula(paste0(along_with, deparse(formula)))
              out_along_with <- mean(.SD[[v]]) + residuals(felm(formula_along_with, .SD))
              list(as.vector(out_v), as.vector(out_along_with))
            }
            , by = c(group), 
            .SDcols = setdiff(names(ans), group)
            ]
            ans_coeff <- ans[, as.list(coef(f(as.formula(paste0(v,"~", along_with)), .SD))), by = c(group), .SDcols = setdiff(names(ans), group)]
            setnames(ans_coeff, c(group, intercept, slope))
            ans[, c(bin) := xtile(get(along_with), n = n), by = c(group)]
            ans2 <- ans[, list(mean(get(along_with)), mean(get(v), na.rm = TRUE)), by = c(group, bin)]
            setnames(ans2, c(group, bin, along_with, v))
              if (!facet){
                if (length(group)){
                  ans_coeff[, (group):= as.factor(get(group))]
                  ans2[, (group):= as.factor(get(group))]
                  g[[i]] <-  ggplot(ans2, aes_string(weight = ww, x = along_with, y = v, color = group)) + geom_point(alpha = 0.6) + geom_abline(data = ans_coeff, aes_string(intercept = intercept, slope = slope, color = group))
                } else{
                  g[[i]] <-  ggplot(ans2, aes_string(weight = ww, x = along_with, y = v)) + geom_point(colour = hcl(h=15,l=65,c=100)) + geom_abline(data = ans_coeff, aes_string(intercept = intercept, slope = slope))
                }
              } else{
                g[[i]] <-  ggplot(ans2, aes_string(weight = ww, x = along_with, y = v)) + geom_point(alpha = 0.6)  + geom_abline(data = ans_coeff, aes_string(intercept = intercept, slope = slope))+ facet_grid(as.formula(paste0(group, "~.")))
              }
            } 
        } else{
        # no along with
          dummy <- is.integer(ans[,get(v)])+ is.character(ans[,get(v)])
          if (dummy) {
            # case of integer: bars
            setkeyv(ans, c(v, group))
            ans[, (v) := as.factor(get(v))]
            if (!facet){
              if (length(group)){
                ans[, (group):= as.factor(get(group))]
              } else{
                group <- NULL
              }
              g[[i]] <-  ggplot(ans, aes_string(weight = ww, x = v, fill = group)) + geom_bar(width = 0.5, position = "dodge")+ coord_flip() + expand_limits(y=0)
            } else{
                g[[i]] <-  ggplot(ans, aes_string(weight = ww, x = v)) + geom_point(stat="bin") + coord_flip() + expand_limits(y = 0)+ facet_grid(as.formula(paste0(group,"~.")))
            }
          } else{ 
            # case of continuous: density
            if (!facet){
              if (length(group)){
                ans[, (group):= as.factor(get(group))]
              }
              g[[i]] <-  ggplot(ans, aes_string(weight = ww, x = v, color = group)) + stat_density(geom = "line", position = "identity") + expand_limits(y=0)
            } else{            
            g[[i]] <-  ggplot(ans, aes_string(weight = ww, x = v)) + stat_density(geom = "line") + facet_grid(as.formula(paste0(group, "~."))) + expand_limits(y=0)
            }
          }
        
      } 
  }
    if (length(g)==1){
      if (verbose){
        print(g[[1]])
      } else{
        suppressWarnings(suppressMessages(print(g[[1]])))
      }
    } else{
      if (verbose){
        do.call(multiplot, g)
      }
      suppressWarnings(suppressMessages(do.call(multiplot, g)))
    }
  }
}
   



# from http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }
 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}



# formula <- ~v4|v1
# along_with <- "v2"
# vars <- v3
# graph(DT, v3, along_with = v2, formula = ~v4|v1, by = id)
# 