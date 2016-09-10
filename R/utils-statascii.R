############################################################
#                                                          #
#                        statascii                         #
#           Create Stata-like ASCII tables in R            #
#                                                          #
############################################################

# library(dplyr)
# library(stringr)

statascii <- function(df, flavor = "oneway", padding = "stata", pad = 1L, ...) {
    ## error checking
    stopifnot(is.data.frame(df))
    if (ncol(df) <= 2L & flavor == "twoway") {
        stop("data.frame must have at least three columns for 'twoway' flavor",
             call. = FALSE)
    }
    if (ncol(df) <= 1L) {
        stop("data.frame must have at least two columns", call. = FALSE)
    }
    ## internal functions
    df <- as.matrix(sapply(format(df, digits = 3L, scientific = FALSE), as.character))
    if (ncol(df) == 1L) {
        df <- t(df)
    }
    if (padding == "stata") {
        colnames(df) <- str_pad(colnames(df), 9L, pad = " ")
    }
    else if (padding == "none") {
    }
    SepLine <- function(n, pad = 1L) {
        tmp <- lapply(n, function(x, pad)
            paste0(rep("\xe2\x94\x80", x + (2L * pad)),
                   collapse = ""),
            pad = pad)
        paste0("\xe2\x94\x80", paste(tmp, collapse = "\xe2\x94\xbc"))
    }
    Row1 <- function(x, n, pad = 1L) {
        foo <- function(i, x, n) {
            fmt <- paste0("%", n[i], "s")
            sprintf(fmt, as.character(x[i]))
        }
        rowc <- sapply(seq_along(x), foo, x = x, n = n)
        paste0(" ",
            paste(paste0(rep(" ", pad), rowc[1], rep(" ", pad)), collapse = ""),
            "\xe2\x94\x82",
            paste(paste0(rep(" ", pad), rowc[-1], rep(" ", pad)), collapse = " ")
        )
    }
    Row2 <- function(x, n, pad = 1L) {
        foo <- function(i, x, n) {
            fmt <- paste0("%", n[i], "s")
            sprintf(fmt, as.character(x[i]))
        }
        rowc <- sapply(seq_along(x), foo, x = x, n = n)
        paste0(
            paste(paste0(rep(" ", pad), rowc[1], rep(" ", pad)), collapse = ""),
            "\xe2\x94\x82",
            paste(paste0(rep(" ", pad), rowc[2:(length(rowc) - 1)], rep(" ", pad)), collapse = ""),
            "\xe2\x94\x82",
            paste(paste0(rep(" ", pad), rowc[length(rowc)], rep(" ", pad)), collapse = " ")
        )
    }
    ## convert everything to characters
    ## nchar in data
    mdf <- apply(df, 2, function(x) max(nchar(x)))
    ## nchar in names
    cnames <- nchar(colnames(df))
    ## max nchar of name+data per elements
    M <- pmax(mdf, cnames)
    M1 <- as.integer(c(M[1], 
                       sum(M[2:(length(M))]) + (3L * ncol(df)) - 6L))
    M2 <- as.integer(c(M[1] - 1L,
                       sum(M[2:(length(M) - 1L)],
                       (2L * ncol(df)) - 6L),
                       M[length(M)] - 1L))
    if (flavor == "oneway") {
        ## write the header
        sep <- SepLine(M1, pad = pad)
        writeLines(Row1(colnames(df), M, pad = pad))
        writeLines(sep)
        ## write the rows
        totalLine <- nrow(df) - 1L
        for (i in seq_len(nrow(df))) {
            ## write a row
            writeLines(Row1(df[i, ], M, pad = pad))
            ## write separator
            if (i == totalLine) {
                writeLines(sep)
            }
        }
    }
    else if (flavor == "twoway") {
        ## write the header
        sep <- SepLine(M2, pad = pad)
        writeLines(Row2(colnames(df), M, pad = pad))
        writeLines(sep)
        ## write the rows
        totalLine <- nrow(df) - 1L
        for (i in seq_len(nrow(df))) {
            ## write a row
            writeLines(Row2(df[i, ], M, pad = pad))
            ## write separator
            if (i == totalLine) {
                writeLines(sep)
            }
        }
    }
    else if (flavor == "summary") {
        ## write the header
        sep <- SepLine(M1, pad = pad)
        writeLines(Row1(colnames(df), M, pad = pad))
        writeLines(sep)
        ## write the rows
        for (i in seq_len(nrow(df))) {
            ## write a row
            writeLines(Row1(df[i, ], M, pad = pad))
        }
    }
    invisible(df)
}

# ## Examples
# # setup
# library(dplyr)
# # 'oneway' flavor for one-way tables of frequencies
# a <- mtcars %>% count(gear) %>% rename(Freq. = n)
# statascii(a, flavor = "oneway")
# # 'oneway' flavor with no Stata-like padding
# a <- mtcars %>% count(gear) %>% rename(Freq. = n)
# statascii(a, flavor = "oneway", padding = "none")
# # 'twoway' flavor for n-way tables of frequencies
# b <- mtcars %>% count(gear, carb, am) %>% rename(Freq. = n)
# statascii(b, flavor = "twoway")
# # 'summary' flavor for summary statistics
# c <- mtcars %>% group_by(gear) %>% summarize(
#     Obs = n(),
#     Mean = mean(gear),
#     "Std. Dev." = sd(gear),
#     Min = min(gear),
#     Max = max(gear)
# )
# statascii(c, flavor = "summary")
# 
# ## Reference
# # `statascii()` borrows heavily  from `asciify()`.
# # The `asciify()` function was written by @gavinsimpson in StackOverflow (https://stackoverflow.com/questions/13011383) and GitHub Gist (https://gist.github.com/gavinsimpson).
# # The `statascii()' function was written by @gvelasq2 in Github Gist (https://gist.github.com/gvelasq2).