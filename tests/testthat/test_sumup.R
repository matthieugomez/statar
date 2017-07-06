library(dplyr)
library(stringr)
library(statar)
context("sumup")


test_that("sum_up", {
  a <- cars %>% sum_up(speed)
  expect_equal(nrow(cars %>% sum_up(speed)), 1)
  expect_equal(nrow(cars %>% group_by(ok = speed %/% 10) %>% sum_up(dist)), 3)
})
