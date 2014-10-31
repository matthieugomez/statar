library(statar)
context("roll")


order_by  = c(1, 2, 4, 7, 8)
along_with  = c(1, 2, 4, 7, 8)
x = c(1, 1, 1, 1, 1)


test_that("lag order_by work", {
   expect_equal(
   roll_lag(x, sum,  n = 1, order_by = order_by),
    c(1, 2, 2, 2, 2))
   expect_equal(
   roll_lag(x, sum,  n = 1, order_by = order_by, closed = c(FALSE, TRUE)),
    c(1, 1, 1, 1, 1))
  expect_equal(
  roll_lag(x, sum,  n = 1, order_by = order_by, closed = c(TRUE, FALSE)),
   c(NA, 1, 1, 1, 1))
   expect_equal(
     roll_lag(x, sum,  n = 1, order_by = order_by, closed = FALSE),
    c(NA, NA, NA, NA, NA))
    expect_equal(
    roll_lag(x, sum,  n = 2, order_by = order_by),
    c(1, 2, 3, 3, 3))
    expect_equal(
    roll_lag(x, sum,  n = 2, order_by = order_by, closed = c(FALSE, TRUE)),
     c(1, 2, 2, 2, 2))
   expect_equal(
   roll_lag(x, sum,  n = 2, order_by = order_by, closed = c(TRUE, FALSE)),
    c(NA, 1, 2, 2, 2))
    expect_equal(
   roll_lag(x, sum,  n = 2, order_by = order_by, closed = FALSE),
     c(NA, 1, 1, 1, 1))
})



test_that("lag along_with works", {  
   expect_equal(
   roll_lag(x, sum,  n = 1, along_with = along_with),
    c(1, 2, 1, 1, 2))
   expect_equal(
   roll_lag(x, sum,  n = 1, along_with = along_with, closed = c(FALSE, TRUE)),
    c(1, 1, 1, 1, 1))
  expect_equal(
  roll_lag(x, sum,  n = 1, along_with = along_with, closed = c(TRUE, FALSE)),
   c(NA, 1, NA, NA, 1))
   expect_equal(
     roll_lag(x, sum,  n = 1, along_with = along_with, closed = FALSE),
    c(NA, NA, NA, NA, NA))

    expect_equal(
    roll_lag(x, sum,  n = 2, along_with = along_with),
    c(1, 2, 2, 1, 2))
    expect_equal(
    roll_lag(x, sum,  n = 2, along_with = along_with, closed = c(FALSE, TRUE)),
     c(1, 2, 1, 1, 2))
   expect_equal(
   roll_lag(x, sum,  n = 2, along_with = along_with, closed = c(TRUE, FALSE)),
    c(NA, 1, 1, NA, 1))
    expect_equal(
   roll_lag(x, sum,  n = 2, along_with = along_with, closed = FALSE),
     c(NA, 1, NA, NA, 1))
  }
)


test_that("order irrelevant", {  
  expect_equal(
  roll_lag(x, sum,  n = 2, along_with = along_with),
  rev(roll_lag(rev(x), sum,  n = 2, along_with = rev(along_with))))
  expect_equal(
  roll_lag(x, sum,  n = 2, order_by = order_by),
  rev(roll_lag(rev(x), sum,  n = 2, order_by = rev(order_by))))
})




test_that("matrix and list input work", {
  x = c(1, 2, 4, 5, 1)
  y = c(1, 2, 1, 3, 1)
expect_equal(
    roll_lag(list(x,y), function(z){cor(z[[1]], z[[2]])},  n = 1, along_with = along_with),
    roll_lag(matrix(c(x,y),length(x)), function(z){cor(z[,1], z[,2])},  n = 1, along_with = along_with)
)})



order_by  = c(1, 2, 4, 7, 8)
along_with  = c(1, 2, 4, 7, 8)
x = c(1, 1, 1, 1, 1)

test_that("lead works", {
expect_equal(
roll_lead(x, sum,  n = 1, along_with = along_with),
c(2, 1, 1, 2, 1))
expect_equal(
roll_lead(x, sum,  n = 1, along_with = along_with, closed = c(TRUE, FALSE)),
c(1, 1, 1, 1, 1))
expect_equal(
roll_lead(x, sum,  n = 1, along_with = along_with, closed = c(FALSE, TRUE)),
c(1, NA, NA, 1, NA))
expect_equal(
roll_lead(x, sum,  n = 1, along_with = along_with, closed = FALSE),
c(NA, NA, NA, NA, NA))
expect_equal(
roll_lead(x, sum,  n = 2, along_with = along_with),
c(2, 2, 1, 2, 1))
expect_equal(
roll_lead(x, sum,  n = 2, along_with = along_with, closed = c(TRUE, FALSE)),
c(2, 1, 1, 2, 1))
expect_equal(
roll_lead(x, sum,  n = 2, along_with = along_with, closed = c(FALSE, TRUE)),
c(1, 1, NA, 1, NA))
expect_equal(
roll_lead(x, sum,  n = 2, along_with = along_with, closed = FALSE),
c(1, NA, NA, 1, NA))
})



