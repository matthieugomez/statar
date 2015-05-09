library(statar)
context("lead/lag")



year <- c(1992,1989, 1991)
value <- c(3.3, 4.1, 4.5)

test_that("basic order", {
  expect_equal(
  	lag(value, 1, order_by = year),
  	c(4.5, NA, 4.1))
 expect_equal(
	lag(value, 1, along_with = year),
	c(4.5, NA, NA))
   expect_equal(
   	lead(value, 1, order_by = year),
   	c(NA, 4.5 , 3.3))
  expect_equal(
 	lead(value, 1, along_with = year),
 	c(NA, NA , 3.3))
})