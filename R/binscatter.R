#' Experimental function to graph a dataset
#' 
#' @param x A data.table.
#' @param ... Variables to include. Defaults to all non-grouping variables. See the \link[dplyr]{select} documentation.
#' @param by Groups within which variables should be ploted.
#' @param type type of graph among "density", "boxplot", "line", or a statistical method (like "lm", "loeless" or "felm")
#' @param x A variable that specifies the x axis. Should be specified when "type" is "line", "lm" or "loeless".
#' @param winsorize Should variables winsorized?
#' @param reorder Should the category with the most count be printed first?
#' @param facet Should different groups graphed in different windows?
#' @param verbose Should warnings (regarding missing values, outliers, etc) be printed?
#' @param .dots Used to work around non-standard evaluation.
#' @param formula to use then type is "felm"
#' @param w Analytical weights (experimental)
#' @param n Number of quantiles to plot
#' @examples
#' library(dplyr)
#' binscatter(iris, Sepal.Width ~ Sepal.Length | Species)
#' binscatter(iris, Sepal.Width ~ Sepal.Length | Species, n = 10)
#' binscatter(group_by(iris, Species), Sepal.Width ~ Sepal.Length, n = 10)
#' @export
binscatter <- function(x, formula, w = NULL, verbose = FALSE, n = 20) {
  binscatter_(x, formula, w = substitute(w), verbose = verbose, n = n)
}

#' @export
#' @rdname binscatter
binscatter_<- function(x, formula, w = NULL, verbose = FALSE, n = 20){
  w <- names(select_vars_(names(x),w))
  byvars <-  vapply(groups(x), as.character, character(1))
  if (length(byvars)>1){
    group <- tempname(x, 1)
    x <- mutate_(x, .dots = setNames(list(~as.factor(group_indices_(x, .dots = byvars))), group))
  } else if (length(byvars)==1){
    group <- byvars
  } else{
    group <- character(0)
  }
  ww <- "ww"
  if (!length(w)){
    x <- mutate_(x, .dots = setNames(list(~1), ww))
  } else{
   x <-  mutate_(x, .dots = setNames(list(interp(~w/sum(w), w = as.name(w))), ww))
  }
  x <- select_(x, .dots = c(all.vars(formula), ww))
  x <- na.omit(x)
  bin <- "bin"
  intercept <- "intercept"
  slope <- "slope"
  ans <- x
  # type boxplot
  F <- Formula(formula)
  F1 <- formula(F, lhs = 1, rhs = 1)

  F2 <- formula(F, lhs = 0, rhs = - 1)
  y <- deparse(F1[[2]])
  x <- deparse(F1[[3]])
  newformula <- as.Formula(~1, F2)
  ans2 <- do_(ans, .dots = interp(~f(y, x, newformula, ww, .), y = y, x= x, newformula = newformula, ww = ww))
  coeff <- do_(group_by_(ans2, .dots = group), .dots = interp(~data.frame(t(coef(lm(formula,  data = . , weights = ww)))), formula = as.formula(paste0(y, "~", x)), ww = as.name(ww)))
  coeff <- setNames(coeff, c(group, intercept, slope))
  ans2 <- mutate_(ans2, .dots = setNames(list(interp(~xtile(x, n, ww), x = as.name(x), ww = as.name(ww))), bin))
  ans2 <- group_by_(ans2, .dots = bin, add = TRUE)
  summary <- summarize_(ans2, .dots = setNames(list(interp(~mean(x), x = as.name(x)),interp(~mean(y), y = as.name(y))), c(x, y)))
  if (length(group)){
    coeff <- ungroup(coeff)
    coeff <- mutate_(coeff, .dots = setNames(list(interp(~as.factor(group), group = as.name(group))), group))
    summary <- ungroup(summary)
    summary <- mutate_(summary, .dots = setNames( list(interp(~as.factor(group), group = as.name(group))), group))
    g <-  ggplot(summary, aes_string(x = x, y = y, color = group)) + geom_point() + geom_abline(data = coeff, aes_string(intercept = intercept, slope = slope, color = group))
  } else{
    g <-  ggplot(summary, aes_string(x = x, y = y)) + geom_point(colour = hcl(h=15,l=65,c=100)) + geom_abline(data = coeff, aes_string(intercept = intercept, slope = slope))
  }
  print(g)
}



f <- function(varname1, varname2, formula, ww, df){
  felm1 <- felm(as.formula(paste0(varname1, deparse(formula))), df, weights = df[[ww]])
  out1 <- mean(df[[varname1]]) + felm1$residuals
  felm2 <- felm(as.formula(paste0(varname2, deparse(formula))), df, weights = df[[ww]])
  out2 <- mean(df[[varname2]]) + felm2$residuals
  setNames(data.frame(out1, out2, df[[ww]]), c(varname1, varname2, ww))
}

