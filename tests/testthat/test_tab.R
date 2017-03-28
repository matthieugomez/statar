library(dplyr)
library(stringr)
library(statar)
context("tab")

# setup
df <- mtcars %>% mutate(cyl = recode(cyl, `6` = NA_real_), vs = recode(vs, `0` = NA_real_))

# confirm tab() works with NA values
test_that("tab() works with NA values", {
  a <- df %>% tab(cyl)
  expect_equal(names(a), c("cyl", "Freq.", "Percent", "Cum."))
  expect_equal(a$cyl, c("4", "8", NA_character_, "Total"))
  expect_equal(a$Freq., c("11", "14", "7", "32"))
  expect_equal(a$Percent, c("34.4", "43.8", "21.9", "100.0"))
  expect_equal(a$Cum., c("34.4", "78.2", "100.0", "\u00a0"))
})

# confirm tab() recalculates percentage columns with na.rm = TRUE
test_that("tab() recalculates percentage columns with na.rm = TRUE", {
  b <- df %>% tab(cyl, na.rm = TRUE)
  expect_equal(names(b), c("cyl", "Freq.", "Percent", "Cum."))
  expect_equal(b$cyl, c("4", "8", "Total"))
  expect_equal(b$Freq., c("11", "14", "25"))
  expect_equal(b$Percent, c("44.0", "56.0", "100.0"))
  expect_equal(b$Cum., c("44.0", "100.0", "\u00a0"))
})

# confirm tab() works when tabulating more than one variable with NA values
test_that("tab() works when tabulating more than one variable with NA values", {
  c <- df %>% tab(cyl, vs)
  expect_equal(names(c), c("cyl", "vs", "Freq.", "Percent", "Cum."))
  expect_equal(c$cyl, c(4, 4, 8, NA, NA))
  expect_equal(c$vs, c(1, NA, NA, 1, NA))
  expect_equal(c$Freq., c(10, 1, 14, 4, 3))
  expect_equal(c$Percent, c("90.9", "9.1", "100.0", "57.1", "42.9"))
  expect_equal(c$Cum., c("90.9", "100.0", "100.0", "57.1", "100.0"))
})