library(statar)
context("join")


library(data.table)
a <- data.table(x = c(1, 1, 2, 3), y = 1:4)
b <- data.table(x = c(1, 2, 2, 4), z = 1:4)

test_that("univariate inner join has all columns, repeated matching rows", {
  j <- join(a, b, "x", kind = "inner")

  expect_equal(names(j), c("x", "y", "z"))
  expect_equal(j$y, c(1, 2, 3, 3))
  expect_equal(j$z, c(1, 1, 2, 3))
})

test_that("univariate left join has all columns, all rows", {
  j1 <- join(a, b, "x", kind = "left")
  j2 <- join(b, a, "x", kind = "left")

  expect_equal(names(j1), c("x", "y", "z"))
  expect_equal(names(j2), c("x", "z", "y"))

  expect_equal(j1$z, c(1, 1, 2, 3, NA))
  expect_equal(j2$y, c(1, 2, 3, 3, NA))
})

test_that("univariate semi join has x columns, matching rows", {
  j1 <- join(a, b, "x", kind = "semi")
  j2 <- join(b, a, "x", kind = "semi")

  expect_equal(names(j1), c("x", "y"))
  expect_equal(names(j2), c("x", "z"))

  expect_equal(j1$y, 1:3)
  expect_equal(j2$z, 1:3)
})

test_that("univariate anti join has x columns, missing rows", {
  j1 <- join(a, b, "x", kind = "anti")
  j2 <- join(b, a, "x", kind = "anti")

  expect_equal(names(j1), c("x", "y"))
  expect_equal(names(j2), c("x", "z"))

  expect_equal(j1$y, 4)
  expect_equal(j2$z, 4)
})

# Bivariate keys ---------------------------------------------------------------

c <- data.table(
  x = c(1, 1, 2, 3),
  y = c(1, 1, 2, 3),
  a = 1:4)
d <- data.table(
  x = c(1, 2, 2, 4),
  y = c(1, 2, 2, 4),
  b = 1:4)

test_that("bivariate inner join has all columns, repeated matching rows", {
  j <- join(c, d, c("x", "y"), kind = "inner")

  expect_equal(names(j), c("x", "y", "a", "b"))
  expect_equal(j$a, c(1, 2, 3, 3))
  expect_equal(j$b, c(1, 1, 2, 3))
})

test_that("bivariate left join has all columns, all rows", {
  j1 <- join(c, d, c("x", "y"), kind = "left")
  j2 <- join(d, c, c("x", "y"), kind = "left")

  expect_equal(names(j1), c("x", "y", "a", "b"))
  expect_equal(names(j2), c("x", "y", "b", "a"))

  expect_equal(j1$b, c(1, 1, 2, 3, NA))
  expect_equal(j2$a, c(1, 2, 3, 3, NA))
})

test_that("bivariate semi join has x columns, matching rows", {
  j1 <- join(c, d, c("x", "y"), kind = "semi")
  j2 <- join(d, c, c("x", "y"), kind = "semi")

  expect_equal(names(j1), c("x", "y", "a"))
  expect_equal(names(j2), c("x", "y", "b"))

  expect_equal(j1$a, 1:3)
  expect_equal(j2$b, 1:3)
})

test_that("bivariate anti join has x columns, missing rows", {
  j1 <- join(c, d, c("x", "y"), kind = "anti")
  j2 <- join(d, c, c("x", "y"), kind = "anti")

  expect_equal(names(j1), c("x", "y", "a"))
  expect_equal(names(j2), c("x", "y", "b"))

  expect_equal(j1$a, 4)
  expect_equal(j2$b, 4)
})

