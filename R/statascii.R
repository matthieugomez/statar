
# format element so that it has a fixed character width of w
format_fixedwidth <- function(x, w = 8L, ispercentage = FALSE) {
	if (is.na(x)) {
		str_pad("NA", width = w, pad = " ")
	}
	else if (typeof(x) == "double"){
		if (ispercentage) {
			# print with 2 digits after decimal point
			fmt = paste0("%", w, ".2f")
			sprintf(fmt, x)
		} else {
			# corresponds to the command di %w.0g x in stata
			# documentation of sprintf format:
			# first number in the format is the minimum width of character to be printred (counting all characters), i.e w.
			# second number is precision. with e and f, number of digits after decimal point. with g, number of digits.
			n = floor(log10(abs(x)))
			if (x == 0) {
				n = 0
			}
			if ((4 - w <= n) &  (n <= w - 3)) {
				# number of characters= precision, - , dot, number of leading zeros
				fmt = paste0("%", w, ".",  w - 2 + min(0, n), "g")
				sprintf(fmt, x)
			} else {
				if (w < 6) {
					stop("width w should be greater than or equal to 6")
				}
				# eg -1.20e-04
				# number of characters = -, the digit before dot, dot, precision, e, -, 2 digits
				fmt = paste0("%", w, ".", max(w - 7, 0), "e") 
				sprintf(fmt, x)
			}
		}
	} else {
		out = str_pad(format(x), width = w, pad = " ", side = "left")
		# if string longer than w, abbreviate it
		if (nchar(out) > w){
			out = paste0(substring(out, 1, w - 2), "~", substring(out, nchar(out), nchar(out)))
		}
		out
	}
}

# format a dataframe so that columns + column names have a fixed character width given by wvec
format_fixedwidth_dataframe <- function(df, wvec) {
	for (i in 1:length(colnames(df))){
		df[[i]] = sapply(df[[i]], format_fixedwidth, w = wvec[i], ispercentage = (colnames(df)[i] %in% c("Percent", "Cum.")))
		colnames(df)[i] = format_fixedwidth(colnames(df)[i], wvec[i])
	}
	df
}

## Reference
# `statascii()` borrows heavily  from `asciify()`.
# The `asciify()` function was written by @gavinsimpson in StackOverflow (https://stackoverflow.com/questions/13011383) and GitHub Gist (https://gist.github.com/gavinsimpson).
# The `statascii()` function was written by @gvelasq2 in Github (https://github.com/gvelasq2/statascii) and Github Gist (https://gist.github.com/gvelasq2).

measure_width <- function(df, n_groups = 1, w = 8L) {
	w_groups = rep(0.0, n_groups)
	for (i in 1:n_groups) {
		if (typeof(df[[i]]) ==  "double") {
		# if tabulate with respect to variable of type double, make sure we get whole variable name
			w_groups[i] = max(nchar(colnames(df)[i]), w)
		} else {
			w_groups[i] = max(nchar(colnames(df)[i]), nchar(format(df[[i]])))
		}
	}
	w_groups
}

add_line <- function(wvec, n_groups) {
	x = sapply(wvec, function(n){paste(rep("\u2500", n), collapse = "")})
	left <- paste(sapply(x[1:n_groups], function(x){paste0(x, "\u2500", "\u253c", "\u2500")}), collapse = "")
	right <- paste(sapply(x[(n_groups + 1):length(x)], function(x){paste0(x, "\u2500")}), collapse = "")
	paste0(left, right)
}

add_dash <- function(wvec, n_groups) {
	x = sapply(wvec, function(n){paste(rep("-", n), collapse = "")})
	left <- paste(sapply(x[1:n_groups], function(x){paste0(x, "-", "\u253c", "-")}), collapse = "")
	right <- paste(sapply(x[(n_groups + 1):length(x)], function(x){paste0(x, "-")}), collapse = "")
	paste0(left, right)
}

add_row <- function(x, n_groups) {
	left <- paste(sapply(x[1:n_groups], function(x){paste0(x, " ", "\u2502", " ")}), collapse = "")
	right <- paste(sapply(x[(n_groups + 1):length(x)], function(x){paste0(x, " ")}), collapse = "")
	paste0(left, right)
}

statascii <- function(df, n_groups = 1, w = 8L) {
	w_groups = measure_width(df, n_groups, w)
	wvec = c(w_groups, rep(w, ncol(df) - n_groups))
	if (sum(wvec) + 3 * n_groups + (length(wvec) - n_groups) > getOption("width")) {
		wvec = pmin(wvec, w)
	}
	if (sum(wvec) + 3 * n_groups + (length(wvec) - n_groups) > getOption("width")) {
		warning("The summary table is too large to be displayed in ASCII")
	} else {
		df <- format_fixedwidth_dataframe(df, wvec)
		df <- as.matrix(df)
		if (ncol(df) == 1L) {
			df <- t(df)
		}		
		if (nrow(df) > 0){
			writeLines(" ")
			writeLines(add_row(colnames(df), n_groups))
			writeLines(add_line(wvec, n_groups))
			for (i in seq_len(nrow(df))) {
				writeLines(add_row(df[i, ], n_groups))
				if ((n_groups >= 2) && (i < nrow(df)) && (df[i, 1] != df[i + 1L, 1])) {
					writeLines(add_dash(wvec, n_groups))
				}
			}
		}
	}
}

