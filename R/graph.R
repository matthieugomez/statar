#' Experimental function to graph a dataset
#' 
#' @param x A data.table.
#' @param ... Variables to include. Defaults to all non-grouping variables. See the \link[dplyr]{select} documentation.
#' @param by Groups within which variables should be ploted.
#' @param reorder Should the category with the most count be printed first?
#' @param facet Should different groups graphed in different windows?
#' @param winsorize Should variables winsorized?
#' @param type type of graph among "density", "boxplot", "line", "lm", "loeless"
#' @param along_with When "type" is "line", "lm" or "loeless", replace x axis by this variable (ie estimate regression models instead of density).
#' @param verbose Should warnings (regarding missing values, outliers, etc) be printed?

#' @examples
#' N <- 10000
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
#' graph(DT, v3, v4, along_with = v2)
#' graph(DT, v3, v4, along_with = v2, by = id, type = "loeless")
#' @export
graph <- function(x, ..., along_with = NULL, by = NULL, w = NULL, reorder = TRUE, winsorize = TRUE, facet = FALSE, size = 1, verbose = FALSE, type = if (is.null(along_with)) "density"
 else "lm") {
  graph_(x, .dots = lazy_dots(...) , along_with = substitute(along_with), by = substitute(by), w = substitute(w), reorder = reorder, winsorize = winsorize, facet = facet, size = size, verbose = verbose, type = type)
}

#' @export
#' @rdname graph
graph_<- function(x, ..., .dots , along_with = NULL, by = NULL, w = NULL, reorder = TRUE, winsorize = TRUE , facet = FALSE, size = 1, verbose = FALSE, type = if (is.null(along_with)) "density"
 else "lm"){
  type <- match.arg(type, c("density", "boxplot", "line", "lm", "loeless"))
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

  assign_var(x, bin, group, count,  variable, value)


  x <- x[, c(byvars, vars, along_with, w), with = FALSE]

  if (!length(w)){
    assign_var(x, w)
    evaldt(x[, .w := 1])
    ww <- NULL
  } else{
    ww <- as.name(paste0(w,"/sum(",w,")"))
  }

  if (winsorize){
    v <- c(byvars, vars, along_with, w)([length(v)>0]
    x[, v := lapply(.SD,function(x){winsorize(x, verbose = FALSE)}), .SDcols = v]
  }

  if (type == "boxplot"){
    theme = theme_set(theme_minimal())
    theme = theme_update(legend.position="top", legend.title=element_blank(), panel.grid.major.x=element_blank())
    theme = theme_update(axis.text.x=element_blank(), axis.ticks.x = element_blank(), axis.line.x = element_blank(), axis.title.x=element_blank())
    theme = theme_update(axis.line.y = element_blank(), axis.title.y=element_blank(), axis.text.y = element_text(colour="grey"), axis.ticks.y= element_line(colour="grey"))
  
    if (length(byvars)){
      if (length(byvars)>1){
          setkeyv(x, byvars)
          evaldt(x[, .group := 0])
          evaldt(x[unique(x), .group := 1])
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
      print(ggplot(x, aes_string(y = value, x = group , weight = w)) + geom_boxplot(outlier.colour = NULL, aes_string(colour = group, fill = group))+  stat_summary(geom = "crossbar", width=0.65, fatten=0, fill = "white", aes_string(colour = group), fun.data =  mean_cl_boot, alpha = 0.3)  + stat_summary(geom = "crossbar", width=0.65, fatten=0, aes_string(color = group), fun.data =  function(x){m <- mean(x); c(ymin = m, ymax = m, y = m)}, alpha = 0.7) + facet_wrap(facets = as.formula(paste0("~",variable)), scales = "free") + stat_summary(geom = "crossbar", width=0.65, fatten=0, color = "white", fun.data =  function(x){m <- median(x, na.rm = TRUE); c(ymin = m, ymax = m, y = m)}, alpha = 0.7))
    } else{
      print(ggplot(x, aes_string(y = value, x = group , weight = w)) + geom_boxplot(outlier.colour = NULL, colour = hcl(h=15,l=65,c=100), fill = hcl(h=15,l=65,c=100), width = 0.5)+  stat_summary(geom = "crossbar", width=0.65/2, fatten=0, color = hcl(h=15,l=65,c=100), fill = "white", fun.data =  mean_cl_boot, alpha = 0.3)+ stat_summary(geom = "crossbar", width=0.65/2, fatten=0, color = hcl(h=15,l=65,c=100), fun.data =  function(x){m <- mean(x); c(ymin = m, ymax = m, y = m)}, alpha = 0.7)  + facet_wrap(facets = as.formula(paste0("~",variable)), scales = "free")  +stat_summary(geom = "crossbar", width=0.65, fatten=0, color = "white", fun.data =  function(x){m <- median(x, na.rm = TRUE); c(ymin = m, ymax = m, y = m)}, alpha = 0.7))
    }
  } else{
    x <- x[, c(byvars, vars, along_with, w), with = FALSE]


  if (!length(byvars)){
    g <- NULL
    i <- 0
      for (v in vars){
        i <- i+1
        if (length(along_with)){
          ans <- evaldt(x[, list(.along_with, .v, .w)])
          nums <- sapply(x, is.numeric)
          nums_name <- names(nums[nums==TRUE])
          vars=intersect(vars,nums_name)
          if (!length(vars)) stop("Please select at least one non-numeric variable", call. = FALSE)
            if (type == "line"){
              g[[i]] <-  ggplot(ans, aes_string(weight = ww, x = along_with, y = v)) + geom_line() 
            } else{
            evaldt(ans[, .bin := .bincode(.along_with, breaks = seq(min(.along_with, na.rm = TRUE), max(.along_with, na.rm = TRUE), length = 20))])
            evaldt(N <- ans[, sum(.w)])
            ans2 <- evaldt( ans[, list(.along_with = mean(.along_with), .v = weighted.mean(.v,  .w, na.rm = TRUE)), by = bin])
            g[[i]] <-  ggplot(ans, aes_string(weight = ww, x = along_with, y = v)) + stat_smooth(method = type) + geom_point(data=ans2, aes_string(x = along_with, y = v)) 
            }
        } else{
        ans <- evaldt(x[, list(.v, .w)])
        dummy <- evaldt(is.integer(ans[,.v]) + is.character(ans[,.v]))
          if (dummy) {
            if (reorder){ 
              ans <- evaldt(ans[, list(.w, .count = .N), by = .v])
              setkeyv(ans,c(count, v))
              evaldt(ans[, .v := factor(.v, levels = unique(.v), ordered = TRUE)])
            } else{
              evaldt(ans[, .v := as.factor(.v)])
            }
              g[[i]] <-  ggplot(ans, aes_string(weight = ww, x = v)) + geom_point(stat="bin") + coord_flip() + expand_limits(y = 0)
          } else{ 
            evaldt(ans[, .bin := .bincode(.v, breaks = seq(min(.v, na.rm = TRUE), max(.v, na.rm = TRUE), length = 100))])
            evaldt(N <- ans[, sum(.w)])
            ans <- evaldt(ans[, list(.v = mean(.v, na.rm = TRUE), count = sum(.w / N, na.rm = TRUE)), by = .bin])
            # g[[i]] <-  ggplot(ans, aes_string(weight = ww, x = v)) + stat_density(geom = "line")
             g[[i]] <-  ggplot(ans, aes_string(x = v, y= "count")) + geom_point()
          }
        }
      } 
  } else{
    if (length(byvars)>1){
      setkeyv(x, byvars)
      evaldt(x[, .group := 0])
      evaldt(x[unique(x), .group := 1])
      evaldt(x[, .group:= cumsum(.group)])
    } else{
      group <- byvars
    }
    g <- NULL
    i <- 0
      for (v in vars){
        ans <- x[, c(group, v, w, along_with), with = FALSE]
        i <- i+1
        if (length(along_with)){
          if (type == "line"){
            if (!facet){
              g[[i]] <-  ggplot(ans, aes_string(weight = ww, x = along_with, y = v, colour = group)) + geom_line() 
            } else{
              g[[i]] <-  ggplot(ans, aes_string(weight = ww, x = along_with, y = v)) + facet_grid(as.formula(paste0(group, "~.")))
            }
          } else{
            evaldt(ans[, .bin := .bincode(.along_with, breaks = seq(min(.along_with, na.rm = TRUE), max(.along_with, na.rm = TRUE), length = 20))])
            evaldt(N <- ans[, sum(.w)])
            ans2 <- evaldt( ans[, list(.along_with = mean(.along_with), .v = weighted.mean(.v,  .w, na.rm = TRUE), .group), by = list(.group, .bin)])
              if (!facet){
                evaldt(ans[, .group:= as.factor(.group)])
                evaldt(ans2[, .group:= as.factor(.group)])
                g[[i]] <-  ggplot(ans, aes_string(weight = ww, x = along_with, y = v, color = group)) + geom_point(data = ans2, aes_string(x = along_with, y = v, color = group), alpha = 0.6) + stat_smooth(method = type)
              } else{
                g[[i]] <-  ggplot(ans, aes_string(weight = ww, x = along_with, y = v)) + geom_point(data = ans2, aes_string(weight = ww, x = along_with, y = v)) + stat_smooth(method = method) + facet_grid(as.formula(paste0(group, "~.")))
              }
            } 
        } else{
          dummy <- evaldt(is.integer(ans[,.v])+ is.character(ans[,.v]))
          if (dummy) {
            # same order across groups
            setkeyv(ans, c(v, group))
            evaldt(ans[, .v := as.factor(.v)])
            if (!facet){
              evaldt(ans[, .group:= as.factor(.group)])
              g[[i]] <-  ggplot(ans, aes_string(weight = ww, x = v, fill = group)) + geom_bar(width = 0.5, position = "dodge")+ coord_flip() 
            } else{
                g[[i]] <-  ggplot(ans, aes_string(weight = ww, x = v)) + geom_point(stat="bin") + coord_flip() + expand_limits(y = 0)+ facet_grid(as.formula(paste0(group,"~.")))
            }
          } else{ 
            if (!facet){
              evaldt(ans[, .group:= as.factor(.group)])
              g[[i]] <-  ggplot(ans, aes_string(weight = ww, x = v, color = group)) + stat_density(geom = "line", position = "identity")
            } else{            
            g[[i]] <-  ggplot(ans, aes_string(weight = ww, x = v)) + stat_density(geom = "line") + facet_grid(as.formula(paste0(group, "~.")))
            }
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
   




multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)

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




#library(ggplot2)
#library(tidyr)
#
#N=1e4; K=100
#DT <- data.table(
#  id = sample(2, N, TRUE),
#  v2 =  sample(1e6, N, TRUE),                        # int in range [1,1e6]
#  v3 =  sample(round(runif(100,max=100),4), N, TRUE), # numeric e.g. 23.5749
#  v4 =  sample(round(runif(100,max=100),4), N, TRUE) # numeric e.g. 23.5749
#)
#DT[, id:= as.factor(id)]
#DT <- gather(DT, variable, value, starts_with("v"))
#
## theme
#theme = theme_set(theme_minimal())
#theme = theme_update(legend.position="top", legend.title=element_blank(), panel.grid.major.x=#element_blank())
#theme = theme_update(axis.text.x=element_blank(), axis.ticks.x = element_blank(), axis.line.x = #element_blank(), axis.title.x=element_blank())
#theme = theme_update(axis.line.y = element_blank(), axis.title.y=element_blank(), axis.text.y = #element_text(colour="grey"), axis.ticks.y= element_line(colour="grey"))
#
#mean.n <- function(x){ return(c(y = median(x)*0.97, label = round(mean(x),2))) }
#
##Data
#ggplot(DT, mapping=aes_string(y = "value", x = "id")) + geom_boxplot(outlier.colour = NULL, #aes_string(colour="id", fill="id"))  +  stat_summary(geom = "crossbar", width=0.65, fatten=0, #aes_string(colour = "id"), fill = "white", fun.data =  mean_cl_boot)   + facet_wrap(facets = ~# variable, scales = "free") 
#
#+ stat_summary(geom = "crossbar", width=0.65, fatten=0, aes_string(colour = "id"), fun.data =  function(x){m <- mean(x); c(y=m,ymin=m,ymax=m)})


