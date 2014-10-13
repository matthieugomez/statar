#' Experimental function to graph a dataset
#' 
#' @param x A data.table.
#' @param ... Variables to include. Defaults to all non-grouping variables. See the \link[dplyr]{select} documentation.
#' @param along_with Replace x axis by this variable (ie estimate regression models instead of density).
#' @param by Groups within which variables should be ploted.
#' @param reorder Should the category with the most count be printed first?
#' @param facet Should different groups graphed in different windows?
#' @param size Size of points when more than 1000 points by group
#' @param winsorize Should variables winsorized?
#' @param method A character for regression model (lm, loess) when along_with is specified
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
graph <- function(x, ..., along_with = NULL, by = NULL, w = NULL, reorder = TRUE, winsorize = TRUE, facet = FALSE, size = 1, verbose = FALSE, method = "lm") {
  graph_(x, .dots = lazy_dots(...) , along_with = substitute(along_with), by = substitute(by), w = substitute(w), reorder = reorder, winsorize = winsorize, facet = facet, size = size, verbose = verbose, method = method)
}

#' @export
#' @rdname graph
graph_<- function(x, ..., .dots , along_with = NULL, by = NULL, w = NULL, reorder = TRUE, winsorize = winsorize, facet = FALSE, size = 1, verbose = FALSE, method = "lm") {
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
    vars = intersect(vars,nums_name)
  }
  name_list <- function(x){
    if (length(x)){
      x <- sapply(x, as.name)
    }
    unlist(x)
  }

  if (!length(vars)) stop("Please select at least one non-numeric variable", call. = FALSE)
  
  assign_var(x, bin, group)
  x <- x[, c(byvars, vars, along_with, w), with = FALSE]
  if (!length(w)){
    assign_var(x, w)
    evaldt(x[, .w := 1])
    ww <- NULL
  } else{
    ww <- as.name(paste0(w,"/sum(",w,")"))
  }

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
            if (winsorize){
              evaldt(ans[, .v := winsorize(.v, verbose = verbose)])
              evaldt(ans[, .along_with:= winsorize(.along_with, verbose = verbose)])
            }
            evaldt(ans[, .bin := .bincode(.along_with, breaks = seq(min(.along_with, na.rm = TRUE), max(.along_with, na.rm = TRUE), length = 20))])
            evaldt(N <- ans[, sum(.w)])
            ans2 <- evaldt( ans[, list(.along_with = mean(.along_with), .v = weighted.mean(.v,  .w, na.rm = TRUE)), by = bin])
            g[[i]] <-  ggplot(ans, aes_string(weight = ww, x = along_with, y = v)) + stat_smooth(method = method) + geom_point(data=ans2, aes_string(x = along_with, y = v)) 
        } else{
        ans <- evaldt(x[, list(.v, .w)])
        print(ans)
        dummy <- evaldt(is.integer(ans[,.v]) + is.character(ans[,.v]))
          if (dummy) {
            if (!reorder){
              g[[i]] <-  ggplot(x, aes_string(weight = ww, x = v)) + geom_point(stat="bin")+ coord_flip()
            } else{
              ans <- evaldt(ans[, list(.w, N = .N), by = .v])
              setkeyv(ans,c("N", v))
              ans <- evaldt(ans[, .v := factor(.v, levels = unique(.v), ordered = TRUE)])
              g[[i]] <-  ggplot(ans, aes_string(weight = ww, x = v)) + geom_point(stat="bin") + coord_flip()
            }
          } else{ 
            if (winsorize){
              evaldt(ans[, .v:= winsorize(.v, verbose = verbose)])
            }
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
      x[, .group := 0]
      x[unique(x), .group := 1]
      evaldt(x[, .group:= cumsum(.v)])
    } else{
      group <- byvars
    }
    g <- NULL
    i <- 0
      for (v in vars){
        ans <- x[, c(group, v, w, along_with), with = FALSE]
        i <- i+1
        if (length(along_with)){
          if (winsorize){
            evaldt(ans <- ans[, list(.group, .along_with = winsorize(.along_with, verbose = verbose), .v = winsorize(.v, verbose = verbose), w)])
          } 
          evaldt(ans[, .bin := .bincode(along_with, breaks = seq(min(along_with, na.rm = TRUE), max(along_with, na.rm = TRUE), length = 20))])
          evaldt(N <- ans[, sum(w)])
          ans2 <- evaldt( ans[, list(.along_with = mean(.along_with), .v = weighted.mean(.v,  .w, na.rm = TRUE), .group), by = list(.group, .bin)])
            if (!facet){
              evaldt(ans[, .group:= as.factor(.group)])
              evaldt(ans2[, .group:= as.factor(.group)])
              g[[i]] <-  ggplot(ans, aes_string(weight = ww, x = along_with, y = v, color = group)) + geom_point(data = ans2, aes_string(x = along_with, y = v, color = group), alpha = 0.6) + stat_smooth(method = method)
            } else{
              g[[i]] <-  ggplot(ans, aes_string(weight = ww, x = along_with, y = v)) + geom_point(data = ans2, aes_string(weight = ww, x = along_with, y = v), size = size) + stat_smooth(method = method) + facet_grid(as.formula(paste0(group, "~.")))
            } 
        } else{
          dummy <- evaldt(is.integer(ans[,.v])+ is.character(ans[,.v]))
          if (dummy) {
            if (!facet){
              evaldt(ans[, .group:= as.factor(.group)])
              evaldt(ans[, .v:= as.factor(.v)])
              g[[i]] <-  ggplot(ans, aes_string(weight = ww, x = v, fill = group)) + geom_bar(width=.5,position = "dodge")+ coord_flip() 
            } else{
              if (!reorder){
                  g[[i]] <-  ggplot(ans, aes_string(weight = ww, x = v)) + geom_bar(width=.5)+ coord_flip()+ facet_grid(as.formula(paste0(group,"~.")))
      
              } else{
                  ans <- evaldt(ans[, list(N = as.integer(rep(.N,.N)), .w = as.name(.w)), by = c(.group,.v)])
                  setkeyv(ans, c(group, "N",v))
                  ans <- evaldt(ans[, .v := factor(.v, levels = unique(.v), ordered = TRUE)])
                  g[[i]] <-  ggplot(ans, aes_string(weight = ww, x = v)) + geom_bar(width=.5) + coord_flip() + facet_grid(as.formula(paste0(group, "~.")))                 
              }
            }
          } else{ 
            if (winsorize){
              evaldt(ans <- ans[, list(.group, .w, .v = winsorize(.v, verbose = verbose))])
            } 
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






