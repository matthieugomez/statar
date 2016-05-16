library(statar)
context("lead/lag")



year <- c(1992,1989, 1991)
value <- c(3.3, 4.1, 4.5)

test_that("basic order", {
 expect_equal(
	tlag(value, 1, year),
	c(4.5, NA, NA))
  expect_equal(
 	tlead(value, 1, year),
 	c(NA, NA , 3.3))
})

year <- c(1992,1992, 1991)
value <- c(3.3, 4.1, 4.5)
test_that("unique", {
expect_that(tlag(value, 1, year), throws_error())
})