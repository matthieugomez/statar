library(dplyr)
library(stringr)
library(statar)
context("ascii table")

test_that("format_fixedwidth gives a number with w character", {
  expect_equal(format_fixedwidth(-2.3820093234*10^(-10), 8), "-2.4e-10")
  expect_equal(format_fixedwidth(-2.3820093234*10^(-9), 8),  "-2.4e-09")
  expect_equal(format_fixedwidth(-2.3820093234*10^(-8), 8),  "-2.4e-08")
  expect_equal(format_fixedwidth(-2.3820093234*10^(-7), 8),  "-2.4e-07")
  expect_equal(format_fixedwidth(-2.3820093234*10^(-6), 8),  "-2.4e-06")
  expect_equal(format_fixedwidth(-2.3820093234*10^(-5), 8),  "-2.4e-05")
  expect_equal(format_fixedwidth(-2.3820093234*10^(-4), 8),  "-0.00024")
  expect_equal(format_fixedwidth(-2.3820093234*10^(-3), 8),  "-0.00238")
  expect_equal(format_fixedwidth(-2.3820093234*10^(-2), 8),  "-0.02382")
  expect_equal(format_fixedwidth(-2.3820093234*10^(-1), 8),  " -0.2382")
  expect_equal(format_fixedwidth(-2.3820093234*10^(0), 8),   "-2.38201")
  expect_equal(format_fixedwidth(-2.3820093234*10^(1), 8),   "-23.8201")
  expect_equal(format_fixedwidth(-2.3820093234*10^(2), 8),   "-238.201")
  expect_equal(format_fixedwidth(-2.3820093234*10^(3), 8),   "-2382.01")
  expect_equal(format_fixedwidth(-2.3820093234*10^(4), 8),   "-23820.1")
  expect_equal(format_fixedwidth(-2.3820093234*10^(5), 8),   " -238201")
  expect_equal(format_fixedwidth(-2.3820093234*10^(6), 8),   "-2.4e+06")
  expect_equal(format_fixedwidth(-2.3820093234*10^(7), 8),   "-2.4e+07")
  expect_equal(format_fixedwidth(-2.3820093234*10^(8), 8),   "-2.4e+08")
  expect_equal(format_fixedwidth(-2.3820093234*10^(9), 8),   "-2.4e+09")
  expect_equal(format_fixedwidth(-2.3820093234*10^(10), 8),  "-2.4e+10")
})


test_that("format_fixedwidth gives a number with w character", {
  expect_equal(format_fixedwidth(-2.3824633234*10^(-10), 8), "-2.4e-10")
  expect_equal(format_fixedwidth(-2.3824633234*10^(-9), 8),  "-2.4e-09")
  expect_equal(format_fixedwidth(-2.3824633234*10^(-8), 8),  "-2.4e-08")
  expect_equal(format_fixedwidth(-2.3824633234*10^(-7), 8),  "-2.4e-07")
  expect_equal(format_fixedwidth(-2.3824633234*10^(-6), 8),  "-2.4e-06")
  expect_equal(format_fixedwidth(-2.3824633234*10^(-5), 8),  "-2.4e-05")
  expect_equal(format_fixedwidth(-2.3824633234*10^(-4), 8),  "-0.00024")
  expect_equal(format_fixedwidth(-2.3824633234*10^(-3), 8),  "-0.00238")
  expect_equal(format_fixedwidth(-2.3824633234*10^(-2), 8),  "-0.02382")
  expect_equal(format_fixedwidth(-2.3824633234*10^(-1), 8),  "-0.23825")
  expect_equal(format_fixedwidth(-2.3824633234*10^(0), 8),   "-2.38246")
  expect_equal(format_fixedwidth(-2.3824633234*10^(1), 8),   "-23.8246")
  expect_equal(format_fixedwidth(-2.3824633234*10^(2), 8),   "-238.246")
  expect_equal(format_fixedwidth(-2.3824633234*10^(3), 8),   "-2382.46")
  expect_equal(format_fixedwidth(-2.3824633234*10^(4), 8),   "-23824.6")
  expect_equal(format_fixedwidth(-2.3824633234*10^(5), 8),   " -238246")
  expect_equal(format_fixedwidth(-2.3824633234*10^(6), 8),   "-2.4e+06")
  expect_equal(format_fixedwidth(-2.3824633234*10^(7), 8),   "-2.4e+07")
  expect_equal(format_fixedwidth(-2.3824633234*10^(8), 8),   "-2.4e+08")
  expect_equal(format_fixedwidth(-2.3824633234*10^(9), 8),   "-2.4e+09")
  expect_equal(format_fixedwidth(-2.3824633234*10^(10), 8),  "-2.4e+10")
})


test_that("format_fixedwidth does not have trailing zeros", {
  expect_equal(format_fixedwidth(50.5, 8),                   "    50.5")
})

test_that("format_fixedwidth works with 0", {
  expect_equal(format_fixedwidth(0),                         "       0")
})

test_that("format_fixedwidth works with NA", {
  expect_equal(format_fixedwidth(NA, 8),                     "      NA")
})

test_that("format_fixedwidth works with factors", {
  expect_equal(format_fixedwidth(as.factor("month")),        "   month")
})

test_that("format_fixedwidth works with factors", {
  expect_equal(format_fixedwidth(as.factor("month")),        "   month")
})

test_that("format_fixedwidth correctly abbreviates", {
  expect_equal(format_fixedwidth("very long string"),        "very l~g")
})


df <- data_frame(
  check_rightalign = as.factor(c("Lorem ipsum dolor", "sit amet, consectetur adipiscing elit,", "sed do eiusmod tempor")),
  longvarname = c(0, 1, 2)
)
tab(df, check_rightalign)
tab(df, longvarname)



