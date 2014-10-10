#' Experimental function to graph a dataset
#' 
#' @param DT A tbl_dt or tbl_grouped_dt.
#' @param ... Variables to include/exclude. Defaults to all non-grouping variables. See the \link[dplyr]{select} documentation.
#' @param along_with Replace x axis from percentiles by this variable.
#' @param by Groups within which variables should be ploted.
#' @param reorder Should the value of strings with the most count be printed first?
#' @param winsorize Should numeric variables winsorized?
#' @param facet Should results graphed in different windows for each group?
#' @param size Point sizes when more than 1000 points by group
#' @examples
#' N <- 10000
#' DT <- data.table(
#'   id = sample(c("id1","id2","id3"), N, TRUE),
#'   v1 = sample(5, N, TRUE),
#'   v2 = sample(runif(100, max=100), N, TRUE)
#' )
#' DT[, v3 := v2 + rnorm(N, sd = 20)]
#' graph(DT)
#' graph(DT, by = v1)
#' graph(DT, by = v1, facet = TRUE)
#' graph(DT, v3, along_with = v2, by = id)
#' @export
graph <- function(x, ..., along_with = NULL, by = NULL, reorder = TRUE, winsorize = TRUE, facet = FALSE, size = 1) {
  graph_(x, .dots = lazy_dots(...) , along_with = substitute(along_with), by = substitute(by), d = d, reorder = reorder, winsorize = winsorize, facet = facet, size = size)
}

#' @export
#' @rdname graph
graph_<- function(x, ..., .dots , along_with = NULL, by = NULL, d = FALSE, reorder = TRUE, winsorize = winsorize, facet = FALSE, size = 1) {
  stopifnot(is.data.table(x))
  along_with <- names(select_vars_(names(x), along_with))
  byvars <- names(select_vars_(names(x), by))
  dots <- all_dots(.dots, ...)
  vars <- names(select_vars_(names(x), dots, exclude = byvars))
  if (length(vars) == 0) {
     vars <- setdiff(names(x), c(byvars, along_with))
  }
  if (length(along_with)){
    nums <- sapply(x, is.numeric)
    nums_name <- names(nums[nums==TRUE])
    vars=intersect(vars,nums_name)
  }
  if (!length(vars)) stop("Please select at least one non-numeric variable", call. = FALSE)
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
              eval(substitute(ans <- x[, list(winsorize(along_with, verbose = FALSE), winsorize(v, verbose = FALSE))], list(along_with = as.name(along_with), v= as.name(v))))
              setnames(ans,c(along_with,v))
            } else{
              eval(substitute(ans <- x[, list(along_with,v)], list(v= as.name(v), along_with = as.name(along_with))))
            }
            if (nrow(ans)>1000){ 
              ans2 <- sample_n(ans, 1000)
              g[[i]] <-  ggplot(ans, aes_string(x = along_with, y = v)) + geom_point(data =ans2, aes_string(x = along_with, y = v), size = size) + stat_smooth(method = "loess")
            } else{
              g[[i]] <-  ggplot(ans, aes_string(x = along_with, y = v)) + geom_point(size = size) + stat_smooth(method = "loess")
            }
        } else{
        dummy <- eval(substitute(is.integer(x[,v])+ is.character(x[,v]), list(v = as.name(v))))
          if (dummy) {
            if (!reorder){
              g[[i]] <-  ggplot(x, aes_string(x = v)) + geom_bar(width=.5)+ coord_flip()
            } else{
              ans <- eval(substitute(x[, list(N = as.integer(rep(.N,.N))), by = v]))
              setkeyv(ans, c("N", v))
              ans <- eval(substitute(ans[, v := factor(v, levels = unique(v), ordered = TRUE)], list( v= as.name(v))))
              g[[i]] <-  ggplot(ans, aes_string(x = v)) + geom_bar(width=.5) + coord_flip()
            }
          } else{ 
            if (winsorize){
              eval(substitute(ans <- x[, list(winsorize(v, verbose = FALSE))], list(v= as.name(v))))
              setnames(ans,v)
            } else{
              eval(substitute(ans <- x[, list(v)], list(v= as.name(v))))
            }
            g[[i]] <-  ggplot(ans, aes_string(x = v)) + stat_density(geom = "line")
          }
        }
      } 
    if (length(g)==1){
      print(g[[1]])
    } else{
      do.call(multiplot, g)
    }
  } else{
    x <- x[,c(byvars, vars, along_with), with = FALSE]
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
        ans <- x[,c(byvars, v, along_with), with = FALSE]
        i <- i+1
        if (length(along_with)){
          if (winsorize){
            eval(substitute(ans_ans <- ans[, list(group, winsorize(along_with, verbose = FALSE), winsorize(v, verbose = FALSE))], list(group = as.name(group), along_with = as.name(along_with), v= as.name(v))))
            setnames(ans, c(group, along_with, v))
          } else{
            eval(substitute(ans <- ans[, list(group, along_with, v)], list(group = as.name(group), v= as.name(v), along_with = as.name(along_with))))
          }
          if (nrow(ans) > 1000 * length(unique(group))){ 
            ans2 <- ans %>% group_by_(group) %>% sample_n(size = 1000, replace = TRUE)
            if (!facet){
              eval(substitute(ans[, group:= as.factor(group)], list(group = as.name(group))))
              eval(substitute(ans2[, group:= as.factor(group)], list(group = as.name(group))))
              g[[i]] <-  ggplot(ans, aes_string(x = along_with, y = v, color = group)) + geom_point(data = ans2, aes_string(x = along_with, y = v, color = group), size = size) + stat_smooth(method = "loess")
            } else{
              g[[i]] <-  ggplot(ans, aes_string(x = along_with, y = v)) + geom_point(data =ans2, aes_string(x = along_with, y = v), size = size) + stat_smooth(method = "loess") + facet_grid(as.formula(paste0(group, "~.")), scale = "free")
            }
          } else{
            if (!facet){
              eval(substitute(ans[, group:= as.factor(group)], list(group = as.name(group))))
              g[[i]] <-  ggplot(ans, aes_string(x = along_with, y = v, color = group)) + geom_point() + stat_smooth(method = "loess")
            } else{
              g[[i]] <-  ggplot(ans, aes_string(x = along_with, y = v)) + geom_point() + stat_smooth(method = "loess") + facet_grid(as.formula(paste0(group, "~.")), scale = "free")
            }
          }
        } else{
          dummy <- eval(substitute(is.integer(ans[,v])+ is.character(ans[,v]), list(v = as.name(v))))
          if (dummy) {
            if (!facet){
              eval(substitute(ans[, group:= as.factor(group)], list(group = as.name(group))))
              eval(substitute(ans[, v:= as.factor(v)], list(v = as.name(v))))
              g[[i]] <-  ggplot(ans, aes_string(x = v, fill = group)) + geom_bar(width=.5,position = "dodge")+ coord_flip() 
            } else{
              if (!reorder){
                  g[[i]] <-  ggplot(x, aes_string(x = v)) + geom_bar(width=.5)+ coord_flip()+ facet_grid(as.formula(paste0(group,"~.")))
      
              } else{
                  ans <- eval(substitute(x[, list(N = as.integer(rep(.N,.N))), by = c(group,v)]))
                  setkeyv(ans, c(group, "N",v))
                  ans <- eval(substitute(ans[, v := factor(v, levels = unique(v), ordered = TRUE)], list( v= as.name(v))))
                  g[[i]] <-  ggplot(ans, aes_string(x = v)) + geom_bar(width=.5) + coord_flip() + facet_grid(as.formula(paste0(group, "~.")), scale = "free")                 
              }
            }
          } else{ 
            if (winsorize){
              eval(substitute(ans <- ans[, list(group, v = winsorize(v, verbose = FALSE))], list(group = as.name(group), v= as.name(v))))
              setnames(ans,c(group,v))
            } else{
              eval(substitute(ans <- ans[, list(group, v)], list(group = as.name(group), v= as.name(v))))
            }
            if (!facet){
              eval(substitute(ans[, group:= as.factor(group)], list(group = as.name(group))))
              g[[i]] <-  ggplot(ans, aes_string(x = v, color = group)) + stat_density(geom = "line", position = "identity")
            } else{            
            g[[i]] <-  ggplot(ans, aes_string(x = v)) + stat_density(geom = "line") + facet_grid(as.formula(paste0(group, "~.")), scale = "free")
            }
          }
        }
      } 
    if (length(g)==1){
      print(g[[1]])
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
