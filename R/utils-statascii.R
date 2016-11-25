############################################################
#                                                          #
#                        statascii                         #
#           Create Stata-like ASCII tables in R            #
#                                                          #
############################################################

tbl_wrap <- function(tbl, M = M, M1 = M1, width = getOption("width")) {
    stopifnot(is.matrix(tbl))
    if (max(nchar(tbl)) <= width) {
        cat(tbl, sep = "\n")
    }
    if (max(nchar(tbl)) > width) {
        M_rest <- M[-1]+3L
        M_rest[1] <- M_rest[1]-1L
        M_start <- M[-1]
        M_start[seq_along(M_start)] <- 0L
        M_start[1] <- 1L
        M_end <- M[-1]
        M_end[seq_along(M_end)] <- 0L
        M_end[1] <- M_rest[1]
        if (length(M_rest) > 1L) {
            for (i in 2:length(M_rest)) {
                M_end[i] <- M_end[i-1L] + M_rest[i]
                M_start[i] <- M_end[i-1L] + 1L
            }
        }
        col1 <- as.matrix(str_sub(tbl, start = 1L, end = M1[1]+4L))
        col_rest <- as.matrix(str_sub(tbl, start = M1[1]+5L, end = -1L))
        pos <- matrix(c(M_start, M_end), ncol = 2)
        all_cols <- list()
        if (length(M_rest) > 1L) {
            for (i in 1:length(M_rest)) {
                all_cols[[i+1L]] <- as.matrix(str_sub(col_rest, pos[i,1], pos[i,2]))
            }
        }
        all_cols[[1]] <- col1
        col_widths <- vector(mode = "integer", length = length(all_cols))
        col_sums <- vector(mode = "integer", length = length(all_cols))
        col_index <- vector(mode = "integer", length = length(all_cols))
        wrap_count <- 1L
        for (i in 1:length(all_cols)) {
            col_widths[i] <- max(nchar(all_cols[[i]]))
            col_index[i] <- wrap_count
        }
        col_sums[1L] <- col_widths[1L]
        for (i in 2:length(all_cols)) {
            col_sums[i] <- col_widths[i] + col_sums[i-1L]
            if (col_sums[i] > width) {
                wrap_count <- wrap_count + 1L
                col_sums[i] <- col_widths[1L] + col_widths[i]
            }
            if (wrap_count > 1L) {
                col_index[i] <- wrap_count
            }
        }
        wrapped <- vector(mode = "list", length = wrap_count)
        for (i in 1:length(wrapped)) {
            wrapped[i] <- all_cols[1]
        }
        for (i in 2:length(col_index)) {
            current_list <- col_index[i]
            wrapped[[current_list]] <- as.matrix(paste0(as.matrix(unlist(wrapped[current_list])), as.matrix(unlist(all_cols[i]))))
        }
        for (i in 1:length(wrapped)) {
            cat(wrapped[[i]], sep = "\n")
            if (i < length(wrapped)) {
                cat("\n")
            }
        }
    }
}

statascii <- function(df, flavor = "oneway", padding = "stata", pad = 1L, ...) {
    stopifnot(is.data.frame(df))
    if (ncol(df) <= 2L & flavor == "twoway") {
        stop("data.frame must have at least three columns for 'twoway' flavor",
             call. = FALSE)
    }
    if (ncol(df) <= 1L) {
        stop("data.frame must have at least two columns", call. = FALSE)
    }
    df <- as.matrix(sapply(format(df, digits = 3L, scientific = FALSE), as.character))
    if (ncol(df) == 1L) {
        df <- t(df)
    }
    if (padding == "stata") {
        colnames(df) <- str_pad(colnames(df), 9L, pad = " ")
    }
    if (padding == "sum_up") {
        colnames(df) <- str_pad(colnames(df), 5L, pad = " ")
    }
    else if (padding == "none") {
    }
    SepLine <- function(n, pad = 1L) {
        tmp <- lapply(n, function(x, pad)
            paste0(rep("\u2500", x + (2L * pad)),
                   collapse = ""),
            pad = pad)
        paste0("\u2500", paste0(tmp, collapse = "\u253c"))
    }
    Row1 <- function(x, n, pad = 1L) {
        foo <- function(i, x, n) {
            fmt <- paste0("%", n[i], "s")
            sprintf(fmt, as.character(x[i]))
        }
        rowc <- sapply(seq_along(x), foo, x = x, n = n)
        paste0(" ",
            paste0(paste0(rep(" ", pad), rowc[1], rep(" ", pad)), collapse = ""),
            "\u2502",
            paste0(paste0(rep(" ", pad), rowc[-1], rep(" ", pad)), collapse = " ")
        )
    }
    Row2 <- function(x, n, pad = 1L) {
        foo <- function(i, x, n) {
            fmt <- paste0("%", n[i], "s")
            sprintf(fmt, as.character(x[i]))
        }
        rowc <- sapply(seq_along(x), foo, x = x, n = n)
        paste0(
            paste0(paste0(rep(" ", pad), rowc[1], rep(" ", pad)), collapse = ""),
            "\u2502",
            paste0(paste0(rep(" ", pad), rowc[2:(length(rowc) - 1)], rep(" ", pad)), collapse = ""),
            "\u2502",
            paste0(paste0(rep(" ", pad), rowc[length(rowc)], rep(" ", pad)), collapse = " ")
        )
    }
    mdf <- apply(df, 2, function(x) max(nchar(x)))
    cnames <- nchar(colnames(df))
    M <- pmax(mdf, cnames)
    M1 <- as.integer(c(M[1], 
                       sum(M[2:(length(M))]) + (3L * ncol(df)) - 6L))
    M2 <- as.integer(c(M[1] - 1L,
                       sum(M[2:(length(M) - 1L)],
                       (2L * ncol(df)) - 6L),
                       M[length(M)] - 1L))
    if (flavor == "oneway") {
        sep <- SepLine(M1, pad = pad)
        table <- capture.output(writeLines(Row1(colnames(df), M, pad = pad)))
        table <- as.matrix(rbind(table, capture.output(writeLines(sep))))
        totalLine <- nrow(df) - 1L
        for (i in seq_len(nrow(df))) {
            table <- as.matrix(rbind(table, capture.output(writeLines(Row1(df[i, ], M, pad = pad)))))
            if (i == totalLine) {
                table <- as.matrix(rbind(table, capture.output(writeLines(sep))))
            }
        }
        tbl_wrap(table, M = M, M1 = M1)
    }
    else if (flavor == "twoway") {
        sep <- SepLine(M2, pad = pad)
        table <- capture.output(writeLines(Row2(colnames(df), M, pad = pad)))
        table <- as.matrix(rbind(table, capture.output(writeLines(sep))))
        totalLine <- nrow(df) - 1L
        for (i in seq_len(nrow(df))) {
            table <- as.matrix(rbind(table, capture.output(writeLines(Row2(df[i, ], M, pad = pad)))))
            if (i == totalLine) {
                table <- as.matrix(rbind(table, capture.output(writeLines(sep))))
            }
        }
        tbl_wrap(table, M = M, M1 = M1)
    }
    else if (flavor == "summary") {
        sep <- SepLine(M1, pad = pad)
        table <- capture.output(writeLines(Row1(colnames(df), M, pad = pad)))
        table <- as.matrix(rbind(table, capture.output(writeLines(sep))))
        for (i in seq_len(nrow(df))) {
            table <- as.matrix(rbind(table, capture.output(writeLines(Row1(df[i, ], M, pad = pad)))))
        }
        tbl_wrap(table, M = M, M1 = M1)
    }
    invisible(df)
}

## Reference
# `statascii()` borrows heavily  from `asciify()`.
# The `asciify()` function was written by @gavinsimpson in StackOverflow (https://stackoverflow.com/questions/13011383) and GitHub Gist (https://gist.github.com/gavinsimpson).
# The `statascii()' function was written by @gvelasq2 in Github (https://github.com/gvelasq2/statascii) and Github Gist (https://gist.github.com/gvelasq2).