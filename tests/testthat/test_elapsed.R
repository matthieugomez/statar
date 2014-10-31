library(statar)
library(lubridate)
context("dates")

date <- mdy(c("04/03/1992", "01/04/1992", "03/15/1992"))  

test_that("class is maintained", {
datem <- as.monthly(date)
expect_equal(class(datem + 1), "monthly")
expect_equal(class(seq(datem[1], datem[2])), "monthly")
})

test_that("conversion works", {
	expect_equal(as.POSIXlt(as.quarterly(date)), floor_date(date, "quarter") )
	expect_equal(as.POSIXlt(as.monthly(date)), floor_date(date, "month") )

	expect_equal(as.Date(as.quarterly(date)), as.Date(floor_date(date, "quarter") ))
	expect_equal(as.Date(as.monthly(date)), as.Date(floor_date(date, "month") ))
	})


test_that("rounding", {
	expect_equal(as.POSIXlt(as.monthly(mdy("02/28/1960"))), mdy("02/01/1960"))
	expect_equal(as.POSIXlt(as.quarterly(mdy("02/28/1960"))),mdy("01/01/1960"))
	})

