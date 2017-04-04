############################################################
#                                                          #
#                        statascii                         #
#           Create Stata-like ASCII tables in R            #
#                                                          #
############################################################
## Reference
# `statascii()` borrows heavily  from `asciify()`.
# The `asciify()` function was written by @gavinsimpson in StackOverflow (https://stackoverflow.com/questions/13011383) and GitHub Gist (https://gist.github.com/gavinsimpson).
# The `statascii()` function was written by @gvelasq2 in Github (https://github.com/gvelasq2/statascii) and Github Gist (https://gist.github.com/gvelasq2).


# format to have width of w characters
prettyformat_number <- function(x, w = 8L, ispercentage = FALSE, isinteger = FALSE) {
  if (ispercentage) {
    sprintf("%3.2f", x)
  }
  else if (isinteger){
    fmt = paste0("%", w, ".0f")
    sprintf(fmt, x)
  }
  else if (is.na(x)){
    "NA"
  }
  else if (abs(x) <= 10^(w-2)) {
    fmt = paste0("%", w - 2, ".", w - 2, "g")
    sprintf(fmt, x)
  }
  else{
    fmt = paste0("%5.", max(w - 6, 0), "e") 
    sprintf(fmt, x)
  }
}

# format to have fixed width of w
prettyformat_dataframe <- function(df, w = 8L) {
  for (i in 1:length(colnames(df))){
    if (typeof(df[[i]]) == "double"){
      df[[i]] = sapply(df[[i]], prettyformat_number, w = w, ispercentage = (colnames(df)[i] %in% c("Percent", "Cum.")), isinteger = (colnames(df)[i] %in% c("Obs", "Missing", "Freq.")))
    }
    else{
      df[[i]] = format(df[[i]])
    }
    df[[i]] = substring(str_pad(df[[i]], width = w, pad = " "), 1, w)
    colnames(df)[i] = substring(str_pad(colnames(df)[i], width = w, pad = " "), 1, w)
  }
  df
}

add_line <- function(n1, n2, w = 8L) {
  x = paste(rep("\u2500", w), collapse = "")
  left <- paste(sapply(rep(x, n1), function(x){paste0(x, "\u2500", "\u253c", "\u2500")}), collapse = "")
  right <- paste(sapply(rep(x, n2), function(x){paste0(x, "\u2500")}), collapse = "")
  paste0(left, right)
}

add_dash <- function(n1, n2, w = 8L) {
  x = paste(rep("-", w), collapse = "")
  left <- paste(sapply(rep(x, n1), function(x){paste0(x, "-", "\u253c", "-")}), collapse = "")
  right <- paste(sapply(rep(x, n2), function(x){paste0(x, "-")}), collapse = "")
  paste0(left, right)
}

add_row <- function(x, n1, n2, w = 8L) {
  left <- paste(sapply(x[1:n1], function(x){paste0(x, " ", "\u2502", " ")}), collapse = "")
  right <- paste(sapply(x[(n1+1):(n1 + n2)], function(x){paste0(x, " ")}), collapse = "")
  paste0(left, right)
  }

statascii <- function(df, n_groups = 1, w = 8L) {
  n1 = n_groups
  n2 = ncol(df) - n1
  df <- prettyformat_dataframe(df, w = w)
  df <- as.matrix(df)
  if (ncol(df) == 1L) {
    df <- t(df)
  }
  writeLines(" ")
  writeLines(add_row(colnames(df), n1, n2, w = w))
  writeLines(add_line(n1, n2, w = w))
  for (i in seq_len(nrow(df))) {
    writeLines(add_row(df[i, ], n1, n2, w = w))
    if ((n1 >= 2) && (i < nrow(df)) && (df[i, 1] != df[i + 1L, 1])) {
      writeLines(add_dash(n1, n2, w = w))
    }
  }
  invisible(df)
}
