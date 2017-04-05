
# format a number so that it has a fixed character width of w
format_fixedwidth_number <- function(x, w = 8L, ispercentage = FALSE, isinteger = FALSE) {
  if (w < 6) {
    stop("width w should be greater than or equal to 6")
  }
  if (is.na(x)) {
    str_pad("NA", width = w, pad = " ")
  }
  # documentation for format argument in sprintf:
  # first number in the format is the minimum width of character to be printred (counting all characters), i.e w.
  # second number is precision. with e and f, number of digits after decimal point. with g, number of digits.
  else if (ispercentage) {
    # print with 2 digits after decimal point
    fmt = paste0("%", w, ".2f")
    sprintf(fmt, x)
  }
  else if (isinteger) {
    # print with 0 digits after decimal point
    fmt = paste0("%", w, ".0f")
    sprintf(fmt, x)
  }
  else{
    n = floor(log10(abs(x)))
    if ((4 - w <= n) &  (n <= w - 3)) {
      # number of characters= precision, - , dot, number of leading zeros
      fmt = paste0("%", w, ".",  w - 2 + min(0, n), "g")
      sprintf(fmt, x)
    }
    else{
      # eg -1.20e-04
      # number of characters = -, the digit before dot, dot, precision, e, -, 2 digits
      fmt = paste0("%", w, ".", max(w - 7, 0), "e") 
      sprintf(fmt, x)
    }
  }
}

# format a dataframe so that columns + column names have a fixed character width given by wvec
format_fixedwidth_dataframe <- function(df, wvec) {
  for (i in 1:length(colnames(df))){
    if (typeof(df[[i]]) %in%  c("integer", "double")) {
      df[[i]] = sapply(df[[i]], format_fixedwidth_number, w = wvec[i], ispercentage = (colnames(df)[i] %in% c("Percent", "Cum.")), isinteger = (colnames(df)[i] %in% c("Obs", "Missing", "Freq.")))
    }
    else{
      df[[i]] = substring(str_pad(format(df[[i]]), width = wvec[i], pad = " "), 1, wvec[i])
    }
    colnames(df)[i] = substring(str_pad(colnames(df)[i], width = wvec[i], pad = " "), 1, wvec[i])
  }
  df
}



## Reference
# `statascii()` borrows heavily  from `asciify()`.
# The `asciify()` function was written by @gavinsimpson in StackOverflow (https://stackoverflow.com/questions/13011383) and GitHub Gist (https://gist.github.com/gavinsimpson).
# The `statascii()` function was written by @gvelasq2 in Github (https://github.com/gvelasq2/statascii) and Github Gist (https://gist.github.com/gvelasq2).

measure_width <- function(df, w = 8L) {
  out = rep(0.0, ncol(df))
  for (i in 1:ncol(df)){
    if (typeof(df[[i]]) %in%  c("integer", "double")) {
      out[i] = w
    }
    else{
      out[i] = max(nchar(colnames(df)[i]), nchar(format(df[[i]])))
    }
  }
  out
}

add_line <- function(wvec, n1) {
  x = sapply(wvec, function(n){paste(rep("\u2500", n), collapse = "")})
  left <- paste(sapply(x[1:n1], function(x){paste0(x, "\u2500", "\u253c", "\u2500")}), collapse = "")
  right <- paste(sapply(x[(n1 + 1):length(x)], function(x){paste0(x, "\u2500")}), collapse = "")
  paste0(left, right)
}

add_dash <- function(wvec, n1) {
  x = sapply(wvec, function(n){paste(rep("-", n), collapse = "")})
  left <- paste(sapply(x[1:n1], function(x){paste0(x, "-", "\u253c", "-")}), collapse = "")
  right <- paste(sapply(x[(n1 + 1):length(x)], function(x){paste0(x, "-")}), collapse = "")
  paste0(left, right)
}

add_row <- function(x, n1) {
  left <- paste(sapply(x[1:n1], function(x){paste0(x, " ", "\u2502", " ")}), collapse = "")
  right <- paste(sapply(x[(n1 + 1):length(x)], function(x){paste0(x, " ")}), collapse = "")
  paste0(left, right)
}

statascii <- function(df, n_groups = 1, w = 8L) {
  wvec = measure_width(df, w = w)
  n1 <- n_groups
  n2 <- ncol(df) - n1
  if (sum(wvec) + n1 * 3 + n2 > getOption("width")) {
    wvec[1:n1] = pmin(wvec[1:n1], w)
  }
  if (sum(wvec) + n1 * 3 + n2 > getOption("width")) {
    warning("The summary table is too large to be displayed in ASCII")
  } 
  else {
    df <- format_fixedwidth_dataframe(df, wvec)
    df <- as.matrix(df)
    if (ncol(df) == 1L) {
      df <- t(df)
    }
    writeLines(" ")
    writeLines(add_row(colnames(df), n1))
    writeLines(add_line(wvec, n1))
    for (i in seq_len(nrow(df))) {
      writeLines(add_row(df[i, ], n1))
      if ((n1 >= 2) && (i < nrow(df)) && (df[i, 1] != df[i + 1L, 1])) {
        writeLines(add_dash(wvec, n1))
      }
    }
  }
}

