library(dplyr)
library(stringr)
library(statar)
context("pctile")

x <- c(NA, 1:10)                   
test_that("pctile", {
expect_equal(xtile(x, n = 3),  c(NA, 1, 1, 1, 1, 2, 2, 2, 3, 3, 3))
expect_equal(xtile(x, probs = c(0.3, 0.7)), c(NA, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3))
expect_equal(xtile(x, cutpoints = c(2, 3)),  c(NA, 1, 1, 2, 3, 3, 3, 3, 3, 3, 3))
})