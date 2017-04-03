############################################################
#                                                          #
#                        statascii                         #
#           Create Stata-like ASCII tables in R            #
#                                                          #
############################################################

wrap_tbl <- function(tbl, M = M, M1 = M1, width = getOption("width")) {
  stopifnot(is.matrix(tbl))
  if (max(nchar(tbl)) <= width) {
    cat(tbl, sep = "\n")
  }
  if (max(nchar(tbl)) > width) {
    M_rest <- M[-1] + 3L
    M_rest[1] <- M_rest[1] - 1L
    M_start <- M[-1]
    M_start[seq_along(M_start)] <- 0L
    M_start[1] <- 1L
    M_end <- M[-1]
    M_end[seq_along(M_end)] <- 0L
    M_end[1] <- M_rest[1]
    if (length(M_rest) > 1L) {
      for (i in 2L:length(M_rest)) {
        M_end[i] <- M_end[i - 1L] + M_rest[i]
        M_start[i] <- M_end[i - 1L] + 1L
      }
    }
    col_one <- as.matrix(str_sub(tbl, start = 1L, end = M1[1] + 4L))
    col_rest <- as.matrix(str_sub(tbl, start = M1[1] + 5L, end = -1L))
    col_position <- matrix(c(M_start, M_end), ncol = 2L)
    all_cols <- list()
    if (length(M_rest) > 1L) {
      for (i in 1L:length(M_rest)) {
        all_cols[[i + 1L]] <-
          as.matrix(str_sub(col_rest, col_position[i, 1], col_position[i, 2]))
      }
    }
    all_cols[[1]] <- col_one
    col_widths <- vector(mode = "integer", length = length(all_cols))
    col_sums <- vector(mode = "integer", length = length(all_cols))
    col_index <- vector(mode = "integer", length = length(all_cols))
    wrap_count <- 1L
    for (i in 1L:length(all_cols)) {
      col_widths[i] <- max(nchar(all_cols[[i]]))
      col_index[i] <- wrap_count
    }
    col_sums[1] <- col_widths[1]
    for (i in 2L:length(all_cols)) {
      col_sums[i] <- col_widths[i] + col_sums[i - 1L]
      if (col_sums[i] > width) {
        wrap_count <- wrap_count + 1L
        col_sums[i] <- col_widths[1L] + col_widths[i]
      }
      if (wrap_count > 1L) {
        col_index[i] <- wrap_count
      }
    }
    tbl_wrapped <- vector(mode = "list", length = wrap_count)
    for (i in 1L:length(tbl_wrapped)) {
      tbl_wrapped[i] <- all_cols[1]
    }
    for (i in 2L:length(col_index)) {
      current_list <- col_index[i]
      tbl_wrapped[[current_list]] <- as.matrix(
        paste0(as.matrix(unlist(tbl_wrapped[current_list])),
               as.matrix(unlist(all_cols[i])))
      )
    }
    for (i in 1L:length(tbl_wrapped)) {
      cat(tbl_wrapped[[i]], sep = "\n")
      if (i < length(tbl_wrapped)) {
        cat("\n")
      }
    }
  }
}
statascii <- function(df, ..., flavor = "oneway", padding = "stata", pad = 1L, separators = FALSE, digits = 3) {
  stopifnot(is.data.frame(df))
  if (ncol(df) <= 2L & flavor == "twoway") {
    stop("data.frame must have at least three columns for 'twoway' flavor",
         call. = FALSE)
  }
  if (ncol(df) <= 1L) {
    stop("data.frame must have at least two columns", call. = FALSE)
  }
  df <- as.matrix(sapply(format(df, digits = digits, scientific = FALSE), as.character))
  if (ncol(df) == 1L) {
    df <- t(df)
  }
  df[, 1] <- str_replace_na(df[, 1])
  width <- min(10, (getOption("width") %/% length(colnames(df))) - 4L)
  colnames(df) <- str_pad(colnames(df), width, pad = " ")
  add_line <- function(n, pad = 1L) {
    tmp <- lapply(n, function(x, pad)
      paste0(rep("\u2500", x + (2L * pad)),
             collapse = ""),
      pad = pad)
    paste0("\u2500", paste0(tmp, collapse = "\u253c"))
  }
  add_dash <- function(n, pad = 1L) {
    tmp <- lapply(n, function(x, pad)
      paste0(rep("-", x + (2L * pad)),
             collapse = ""),
      pad = pad)
    paste0("-", paste0(tmp, collapse = "\u253c"))
  }
  add_row_oneway <- function(x, n, pad = 1L) {
    reformat <- function(i, x, n) {
      fmt <- paste0("%", n[i], "s")
      sprintf(fmt, as.character(x[i]))
    }
    row_content <- sapply(seq_along(x), reformat, x = x, n = n)
    paste0(" ",
           paste0(paste0(rep(" ", pad), row_content[1], rep(" ", pad)), collapse = ""),
           "\u2502",
           paste0(paste0(rep(" ", pad), row_content[-1], rep(" ", pad)), collapse = " ")
    )
  }
  add_row_twoway <- function(x, n, pad = 1L) {
    reformat <- function(i, x, n) {
      fmt <- paste0("%", n[i], "s")
      sprintf(fmt, as.character(x[i]))
    }
    row_content <- sapply(seq_along(x), reformat, x = x, n = n)
    paste0(
      paste0(paste0(rep(" ", pad), row_content[1], rep(" ", pad)), collapse = ""),
      "\u2502",
      paste0(paste0(rep(" ", pad), row_content[2:(length(row_content) - 1L)], rep(" ", pad)), collapse = ""),
      "\u2502",
      paste0(paste0(rep(" ", pad), row_content[length(row_content)], rep(" ", pad)), collapse = " ")
    )
  }
  nchar_content <- apply(df, 2, function(x) {
    max(nchar(x, keepNA = FALSE))
  })
  nchar_names <- nchar(colnames(df), keepNA = FALSE)
  M <- pmax(nchar_content, nchar_names)
  M1 <- as.integer(c(M[1],
                     sum(M[2:(length(M))]) + (3L * ncol(df)) - 6L))
  M2 <- as.integer(c(M[1] - 1L,
                     sum(M[2:(length(M) - 1L)],
                     (2L * ncol(df)) - 6L),
                     M[length(M)] - 1L))
  if (flavor == "oneway") {
    table_line <- add_line(M1, pad = pad)
    group_dashes <- add_dash(M1, pad = pad)
    table_captured <-
      capture.output(writeLines(add_row_oneway(colnames(df), M, pad = pad)))
    table_captured <-
      as.matrix(rbind(table_captured, capture.output(writeLines(table_line))))
    total_line <- nrow(df) - 1L
    for (i in seq_len(nrow(df))) {
      table_captured <-
        as.matrix(rbind(table_captured, capture.output(writeLines(add_row_oneway(df[i, ], M, pad = pad)))))
      if (i > 0L & i < total_line) {
        if (separators) {
          if (df[i, 1] != df[i + 1L, 1]) {
            table_captured <-
              as.matrix(rbind(table_captured, capture.output(writeLines(group_dashes))))                                  
          }
        }
      }
      if (i == total_line) {
        table_captured <-
          as.matrix(rbind(table_captured, capture.output(writeLines(table_line))))
      }
    }
    wrap_tbl(table_captured, M = M, M1 = M1)
  }
  else if (flavor == "twoway") {
    table_line <- add_line(M2, pad = pad)
    group_dashes <- add_dash(M2, pad = pad)
    table_captured <-
      capture.output(writeLines(add_row_twoway(colnames(df), M, pad = pad)))
    table_captured <-
      as.matrix(rbind(table_captured, capture.output(writeLines(table_line))))
    total_line <- nrow(df) - 1L
    for (i in seq_len(nrow(df))) {
      table_captured <-
        as.matrix(rbind(table_captured, capture.output(writeLines(add_row_twoway(df[i, ], M, pad = pad)))))
      if (i > 0L & i < total_line) {
        if (separators) {
          if (df[i, 1] != df[i + 1L, 1]) {
            table_captured <-
              as.matrix(rbind(table_captured, capture.output(writeLines(group_dashes))))
          }
        }
      }
      if (i == total_line) {
        table_captured <-
          as.matrix(rbind(table_captured, capture.output(writeLines(table_line))))
      }
    }
    wrap_tbl(table_captured, M = M, M1 = M1)
  }
  else if (flavor == "summary") {
    table_line <- add_line(M1, pad = pad)
    group_dashes <- add_dash(M1, pad = pad)
    table_captured <-
      capture.output(writeLines(add_row_oneway(colnames(df), M, pad = pad)))
    table_captured <-
      as.matrix(rbind(table_captured, capture.output(writeLines(table_line))))
    for (i in seq_len(nrow(df))) {
      table_captured <-
        as.matrix(rbind(table_captured, capture.output(writeLines(add_row_oneway(df[i, ], M, pad = pad)))))
      if (i > 0L & i < nrow(df)) {
        if (separators) {
          if (df[i, 1] != df[i + 1L, 1]) {
            table_captured <-
              as.matrix(rbind(table_captured, capture.output(writeLines(group_dashes))))
          }
        }
      }
    }
    wrap_tbl(table_captured, M = M, M1 = M1)
  }
  invisible(df)
}

## Reference
# `statascii()` borrows heavily  from `asciify()`.
# The `asciify()` function was written by @gavinsimpson in StackOverflow (https://stackoverflow.com/questions/13011383) and GitHub Gist (https://gist.github.com/gavinsimpson).
# The `statascii()` function was written by @gvelasq2 in Github (https://github.com/gvelasq2/statascii) and Github Gist (https://gist.github.com/gvelasq2).