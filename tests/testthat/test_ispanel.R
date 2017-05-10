library(dplyr)
library(stringr)
library(statar)
context("is.panel")

df <- data_frame(
    id1    = c(1, 1, 1, 2, 2),
    id2   = 1:5,
    year  = c(1991, 1993, NA, 1992, 1992),
    value = c(4.1, 4.5, 3.3, 3.2, 5.2)
)
df1 <- df %>% filter(!is.na(year))

test_that("is.panel works", {
  expect_equal(df %>% group_by(id1) %>% is.panel(year), FALSE)
  expect_equal(capture_messages(df %>% group_by(id1) %>% is.panel(year)), c("Variable year has missing values in 1 row(s): 3\n", "Variables (id1 , year) have duplicates for rows (4,5)\n"))     
  expect_equal(df1 %>% is.panel(year), FALSE)
  expect_equal(capture_messages(df1 %>% is.panel(year)), "Variables (year) have duplicates for rows (3,4)\n")
  expect_equal(df1 %>% group_by(id1) %>% is.panel(year), FALSE)
  expect_equal(capture_messages(df1 %>% group_by(id1) %>% is.panel(year)), "Variables (id1 , year) have duplicates for rows (3,4)\n")
  expect_equal(df1 %>% group_by(id1, id2) %>% is.panel(year), TRUE)
})