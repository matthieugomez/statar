library(statar)
context("roll")

along_with  = c(1, 2, 4, 7)
x = c(1, 1, 1, 1)


test_that("1 works", {
  expect_equal(
  	roll_lag(x, sum,  n = 1, along_with = along_with),
 	c(1,2,1,1))
 expect_equal(
	roll_lag(x, sum,  n = 2, along_with = along_with),
	c(1,2,2,1))
 expect_equal(
 	roll_lead(x, sum,  n = 1, along_with = along_with),
	c(2,1,1,1))
 expect_equal(
  	roll_lead(x, sum,  n = 2, along_with = along_with),
 	c(2,2,1,1))
})

