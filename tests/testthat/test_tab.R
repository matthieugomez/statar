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
  expect_equal(a$cyl, c(4, 8, NA))
  expect_equal(a$Freq., c(11, 14, 7))
  expect_equal(a$Percent, c(34.375, 43.750, 21.875))
  expect_equal(a$Cum., c(34.375, 78.125, 100.000))
})

# confirm tab() recalculates percentage columns with na.rm = TRUE
test_that("tab() recalculates percentage columns with na.rm = TRUE", {
  b <- df %>% tab(cyl, na.rm = TRUE)
  expect_equal(names(b), c("cyl", "Freq.", "Percent", "Cum."))
  expect_equal(b$cyl, c(4, 8))
  expect_equal(b$Freq., c(11, 14))
  expect_equal(b$Percent, c(44, 56))
  expect_equal(b$Cum., c(44, 100))
})

# confirm tab() works when tabulating more than one variable with NA values
test_that("tab() works when tabulating more than one variable with NA values", {
  c <- df %>% tab(cyl, vs)
  expect_equal(names(c), c("cyl", "vs", "Freq.", "Percent", "Cum."))
  expect_equal(c$cyl, c(4, 4, 8, NA, NA))
  expect_equal(c$vs, c(1, NA, NA, 1, NA))
  expect_equal(c$Freq., c(10, 1, 14, 4, 3))
  expect_equal(c$Percent, c(31.250,  3.125, 43.750, 12.500,  9.375))
  expect_equal(c$Cum., c(31.250,  34.375,  78.125,  90.625, 100.000))
})