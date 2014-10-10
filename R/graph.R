#' Experimental function to graph a dataset
#' 
#' @param DT A tbl_dt or tbl_grouped_dt.
#' @param ... Variables to include/exclude. Defaults to all non-grouping variables. See the \link[dplyr]{select} documentation.
#' @param by Groups within which variables should be ploted.
#' @param reorder Should the value of strings with the most count be printed first?
#' @param winsorize Should numeric variables winsorized?
#' @param facet Should results graphed in different windows for each group?
#' @examples
#' N <- 100
#' DT <- data.table(
#'   id = sample(sprintf("id%03d",1:3), N, TRUE),
#'   v1 = sample(5, N, TRUE),
#'   v2 =  sample(round(runif(100,max=100),4), N, TRUE)
#' )
#' graph(DT)
#' graph(DT, v2, by = v1)
#' @export
graph <- function(x, ..., by = NULL, reorder = TRUE, winsorize = TRUE, facet = FALSE) {
  graph_(x, .dots = lazy_dots(...) , by = substitute(by), d = d, reorder = reorder, winsorize = winsorize, facet = facet)
}

#' @export
#' @rdname graph
graph_<- function(x, ..., .dots ,by = NULL, d = FALSE, reorder = TRUE, winsorize = winsorize, facet = FALSE) {
  stopifnot(is.data.table(x))
  byvars <- names(select_vars_(names(x), by))
  dots <- all_dots(.dots, ...)
  vars <- names(select_vars_(names(x), dots, exclude = byvars))
  if (length(vars) == 0) {
     vars <- setdiff(names(x), byvars)
  }
  if (!length(byvars)){
    g <- NULL
    i <- 0
      for (v in vars){
        i <- i+1
          dummy <- eval(substitute(is.integer(x[,v])+ is.character(x[,v]), list(v = as.name(v))))
          if (dummy) {
            if (!reorder){
              g[[i]] <-  ggplot(x, aes_string(x = v)) + geom_bar()+ coord_flip()
            } else{
              ans <- eval(substitute(x[, list(N = as.integer(rep(.N,.N))), by = v]))
              setkeyv(ans, c("N", v))
              ans <- eval(substitute(ans[, v := factor(v, levels = unique(v), ordered = TRUE)], list( v= as.name(v))))
              g[[i]] <-  ggplot(ans, aes_string(x = v)) + geom_bar() + coord_flip()
            }
          } else{ 
            if (winsorize){
              eval(substitute(ans <- x[, list(winsorize(v, verbose = TRUE))], list(v= as.name(v))))
              setnames(ans,v)
            } else{
              eval(substitute(ans <- x[, list(v)], list(v= as.name(v))))
            }
            g[[i]] <-  ggplot(ans, aes_string(x = v)) + stat_density(geom = "line")
          }
      } 
    if (length(g)==1){
      print(g)
    } else{
      do.call(multiplot, g)
    }
  } else{
    ans <- x[,c(byvars, vars), with = FALSE]
    if (length(byvars)>1){
      group <- tempname("group", x)
      setkeyv(ans, byvars)
      ans[, (group) := 0]
      ans[unique(ans), (group) := 1]
      eval(substitute(ans[, (group):= cumsum(v)], list(v = as.name(group))))
    } else{
      group <- byvars
    }
    g <- NULL
    i <- 0
      for (v in vars){
        i <- i+1
          dummy <- eval(substitute(is.integer(x[,v])+ is.character(x[,v]), list(v = as.name(v))))
          assign(v,v)
          if (dummy) {
            if (!facet){
              eval(substitute(ans[, group:= as.factor(group)], list(group = as.name(group))))
              eval(substitute(ans[, v:= as.factor(v)], list(v = as.name(v))))
              g[[i]] <-  ggplot(x, aes_string(x = v, fill = group)) + geom_bar(position = "dodge")+ coord_flip() 
            } else{
              if (!reorder){
                  g[[i]] <-  ggplot(x, aes_string(x = v)) + geom_bar()+ coord_flip()+ facet_grid(as.formula(paste0(group,"~.")))
      
              } else{
                ans <- eval(substitute(x[, list(N = as.integer(rep(.N,.N))), by = c(group,v)]))
                setkeyv(ans, c(group, "N",v))
                ans <- eval(substitute(ans[, v := factor(v, levels = unique(v), ordered = TRUE)], list( v= as.name(v))))
                g[[i]] <-  ggplot(ans, aes_string(x = v)) + geom_bar() + coord_flip() + facet_grid(as.formula(paste0(group, "~.")))
              }
            }
          } else{ 
            if (winsorize){
              eval(substitute(ans <- x[, list(group, v = winsorize(v, verbose = TRUE))], list(group = as.name(group), v= as.name(v))))
              setnames(ans,c(group,v))
            } else{
              eval(substitute(ans <- x[, list(group, v)], list(group = as.name(group), v= as.name(v))))
            }
            if (!facet){
              eval(substitute(ans[, group:= as.factor(group)], list(group = as.name(group))))
              g[[i]] <-  ggplot(ans, aes_string(x = v, color = group)) + stat_density(geom = "line", position = "identity")
            } else{            
            g[[i]] <-  ggplot(ans, aes_string(x = v)) + stat_density(geom = "line") + facet_grid(as.formula(paste0(group, "~.")))
            }
          }
      } 
    if (length(g)==1){
      print(g)
    } else{
      do.call(multiplot, g)
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
