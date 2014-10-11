#' Experimental function to graph a dataset
#' 
#' @param DT A tbl_dt or tbl_grouped_dt.
#' @param ... Variables to include/exclude. Defaults to all non-grouping variables. See the \link[dplyr]{select} documentation.
#' @param along_with Replace x axis from percentiles by this variable.
#' @param by Groups within which variables should be ploted.
#' @param w A weight variable
#' @param reorder Should the category with the most count be printed first?
#' @param facet Should results graphed in different windows for each group?
#' @param size Point sizes when more than 1000 points by group
#' @param winsorize Should numeric variables winsorized?
#' @param method A character for regression model (lm, loess)
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
#' graph(DT, v3, v4, along_with = v2)
#' graph(DT, v3, v4, along_with = v2, by = id)
#' @export
graph <- function(x, ..., along_with = NULL, by = NULL, w = NULL, reorder = TRUE, winsorize = TRUE, facet = FALSE, size = 1, verbose = FALSE, method = "loess") {
  graph_(x, .dots = lazy_dots(...) , along_with = substitute(along_with), by = substitute(by), w = substitute(w), d = d, reorder = reorder, winsorize = winsorize, facet = facet, size = size, verbose = verbose, method = method)
}

#' @export
#' @rdname graph
graph_<- function(x, ..., .dots , along_with = NULL, by = NULL, w = NULL, d = FALSE, reorder = TRUE, winsorize = winsorize, facet = FALSE, size = 1, verbose = FALSE, method = "loess") {
  stopifnot(is.data.table(x))
  w <- names(select_vars_(names(x),w))
  along_with <- names(select_vars_(names(x), along_with))
  byvars <- names(select_vars_(names(x), by))
  dots <- all_dots(.dots, ...)
  vars <- names(select_vars_(names(x), dots, exclude = c(byvars,w,along_with)))
  if (length(vars) == 0) {
     vars <- setdiff(names(x), c(byvars, along_with, w))
  }
  if (length(along_with)){
    nums <- sapply(x, is.numeric)
    nums_name <- names(nums[nums==TRUE])
    vars=intersect(vars,nums_name)
  }
  if (!length(vars)) stop("Please select at least one non-numeric variable", call. = FALSE)
  x <- x[,c(byvars, vars, along_with, w), with = FALSE]

  if (!length(w)){
    w <- tempname("weight", x)
    x[, (w) := 1]
    ww <- NULL
  } else{
    ww <- paste0(w,"/sum(",w,")")
  }

  if (!length(byvars)){
    g <- NULL
    i <- 0
      for (v in vars){
        i <- i+1
        if (length(along_with)){
          nums <- sapply(x, is.numeric)
          nums_name <- names(nums[nums==TRUE])
          vars=intersect(vars,nums_name)
          if (!length(vars)) stop("Please select at least one non-numeric variable", call. = FALSE)
            if (winsorize){
              eval(substitute(ans <- x[, list(winsorize(along_with, verbose = verbose), winsorize(v, verbose = verbose), w)], list(along_with = as.name(along_with), v= as.name(v), w = as.name(w))))
              setnames(ans,c(along_with, v, w))
            } else{
              eval(substitute(ans <- x[, list(along_with, w, v)], list(v= as.name(v), along_with = as.name(along_with), w= as.name(w))))
            }
            if (nrow(ans)>1000){ 
              ans2 <- sample_n(ans, 1000)
              g[[i]] <-  ggplot(ans, aes_string(weight = ww, x = along_with, y = v)) + geom_point(data =ans2, aes_string(weight = ww, x = along_with, y = v), size = size) + stat_smooth(method = method)
            } else{
              g[[i]] <-  ggplot(ans, aes_string(weight = ww, x = along_with, y = v)) + geom_point(size = size) + stat_smooth(method = method)
            }
        } else{
        dummy <- eval(substitute(is.integer(x[,v])+ is.character(x[,v]), list(v = as.name(v))))
          if (dummy) {
            if (!reorder){
              g[[i]] <-  ggplot(x, aes_string(weight = ww, x = v)) + geom_bar(width=.5)+ coord_flip()
            } else{
              ans <- eval(substitute(x[, list(w, N = .N), by = v], list(w=as.name(w))))
              setkeyv(ans, c(w, "N", v))
              ans <- eval(substitute(ans[, v := factor(v, levels = unique(v), ordered = TRUE)], list( v= as.name(v))))
              g[[i]] <-  ggplot(ans, aes_string(weight = ww, x = v)) + geom_bar(width=.5) + coord_flip()
            }
          } else{ 
            if (winsorize){
              eval(substitute(ans <- x[, list(winsorize(v, verbose = verbose), w)], list(v= as.name(v), w= as.name(w))))
              setnames(ans,c(v, w))
            } 
            g[[i]] <-  ggplot(ans, aes_string(weight = ww, x = v)) + stat_density(geom = "line")
          }
        }
      } 
  } else{
    if (length(byvars)>1){
      group <- tempname("group", x)
      setkeyv(x, byvars)
      x[, (group) := 0]
      x[unique(x), (group) := 1]
      eval(substitute(x[, (group):= cumsum(v)], list(v = as.name(group))))
    } else{
      group <- byvars
    }
    g <- NULL
    i <- 0
      for (v in vars){
        ans <- x[,c(byvars, v, w, along_with), with = FALSE]
        i <- i+1
        if (length(along_with)){
          if (winsorize){
            eval(substitute(ans <- ans[, list(group, winsorize(along_with, verbose = verbose), winsorize(v, verbose = verbose), w)], list(group = as.name(group), along_with = as.name(along_with), v= as.name(v), w= as.name(w))))
            setnames(ans, c(group, along_with, v, w))
          } else{
            eval(substitute(ans <- ans[, list(group, along_with, v, w)], list(group = as.name(group), v= as.name(v), along_with = as.name(along_with), w= as.name(w))))
          }
          if (nrow(ans) > 1000){ 
            eval(substitute(ans2 <- sample_n(group_by(ans, group), size = round(1000/length(unique(ans[, group]))), replace = TRUE), list(group = as.name(group))))
            if (!facet){
              eval(substitute(ans[, group:= as.factor(group)], list(group = as.name(group))))
              eval(substitute(ans2[, group:= as.factor(group)], list(group = as.name(group))))
              g[[i]] <-  ggplot(ans, aes_string(weight = ww, x = along_with, y = v, color = group)) + geom_point(data = ans2, aes_string(x = along_with, y = v, color = group), size = size) + stat_smooth(method = method)
            } else{
              g[[i]] <-  ggplot(ans, aes_string(weight = ww, x = along_with, y = v)) + geom_point(data =ans2, aes_string(weight = ww, x = along_with, y = v), size = size) + stat_smooth(method = method) + facet_grid(as.formula(paste0(group, "~.")))
            }
          } else{
            if (!facet){
              eval(substitute(ans[, group:= as.factor(group)], list(group = as.name(group))))
              g[[i]] <-  ggplot(ans, aes_string(weight = ww, x = along_with, y = v, color = group)) + geom_point() + stat_smooth(method = method)
            } else{
              g[[i]] <-  ggplot(ans, aes_string(weight = ww, x = along_with, y = v)) + geom_point() + stat_smooth(method = method) + facet_grid(as.formula(paste0(group, "~.")))
            }
          }
        } else{
          dummy <- eval(substitute(is.integer(ans[,v])+ is.character(ans[,v]), list(v = as.name(v))))
          if (dummy) {
            if (!facet){
              eval(substitute(ans[, group:= as.factor(group)], list(group = as.name(group))))
              eval(substitute(ans[, v:= as.factor(v)], list(v = as.name(v))))
              g[[i]] <-  ggplot(ans, aes_string(weight = ww, x = v, fill = group)) + geom_bar(width=.5,position = "dodge")+ coord_flip() 
            } else{
              if (!reorder){
                  g[[i]] <-  ggplot(ans, aes_string(weight = ww, x = v)) + geom_bar(width=.5)+ coord_flip()+ facet_grid(as.formula(paste0(group,"~.")))
      
              } else{
                  ans <- eval(substitute(ans[, list(N = as.integer(rep(.N,.N)), w= as.name(w)), by = c(group,v)]))
                  setkeyv(ans, c(group, "N",v))
                  ans <- eval(substitute(ans[, v := factor(v, levels = unique(v), ordered = TRUE)], list( v= as.name(v))))
                  g[[i]] <-  ggplot(ans, aes_string(weight = ww, x = v)) + geom_bar(width=.5) + coord_flip() + facet_grid(as.formula(paste0(group, "~.")))                 
              }
            }
          } else{ 
            if (winsorize){
              eval(substitute(ans <- ans[, list(group, w, v = winsorize(v, verbose = verbose))], list(group = as.name(group), v= as.name(v), w= as.name(w))))
              setnames(ans,c(group, w, v))
            } 
            if (!facet){
              eval(substitute(ans[, group:= as.factor(group)], list(group = as.name(group))))
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
