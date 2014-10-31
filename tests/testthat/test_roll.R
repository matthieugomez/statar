library(statar)
context("roll")

along_with  = c(1, 2, 4, 7, 8)
x = c(1, 1, 1, 1, 1)


test_that("lag works", {
expect_equal(
  	roll_lag(x, sum,  n = 1, along_with = along_with),
 	c(1, 2, 1, 1, 2))
  expect_equal(
  	roll_lag(x, sum,  n = 1, along_with = along_with, closed.left = FALSE),
 	c(1, 1, 1, 1, 1))
  expect_equal(
 	roll_lag(x, sum,  n = 2, along_with = along_with),
 	c(1, 2, 2, 1, 2))
  expect_equal(
 	roll_lag(x, sum,  n = 2, along_with = along_with, closed.left = FALSE),
 	c(1, 2, 1, 1, 2))
})


test_that("lead works", {
expect_equal(
  	roll_lead(x, sum,  n = 1, along_with = along_with),
 	c(2, 1, 1, 2, 1))
  expect_equal(
  	roll_lead(x, sum,  n = 1, along_with = along_with, closed.righ = FALSE),
 	c(1, 1, 1, 1, 1))
  expect_equal(
 	roll_lead(x, sum,  n = 2, along_with = along_with),
 	c(2, 2, 1, 2, 1))
  expect_equal(
 	roll_lead(x, sum,  n = 2, along_with = along_with, closed.righ = FALSE),
 	c(2, 1, 1, 2, 1))
})


x = c(1, 2, 4, 5, 1)
y = c(1, 2, 1, 3, 1)
test_that("matrix and list", {
expect_equal(
  	roll_lag(list(x,y), function(z){cor(z[[1]], z[[2]])},  n = 1, along_with = along_with),
  	roll_lag(matrix(c(x,y),length(x)), function(z){cor(z[,1], z[,2])},  n = 1, along_with = along_with)
)})

