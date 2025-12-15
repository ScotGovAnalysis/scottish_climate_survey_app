# tests/testthat/test-factors.R
# Unit tests for factor-related functions

library(testthat)
library(dplyr)

test_that("make_factor creates factor with labels", {
  vallab <- tibble::tibble(
    variable = c("q1", "q1", "q1"),
    value = c("1", "2", "3"),
    label = c("Low", "Medium", "High")
  )
  
  x <- c("1", "2", "3", "1")
  result <- make_factor(x, "q1", vallab)
  
  expect_s3_class(result, "factor")
  expect_equal(levels(result), c("Low", "Medium", "High"))
  expect_equal(as.character(result), c("Low", "Medium", "High", "Low"))
})

test_that("make_factor handles missing labels", {
  vallab <- tibble::tibble(
    variable = character(0),
    value = character(0),
    label = character(0)
  )
  
  x <- c("1", "2", "3")
  result <- make_factor(x, "q1", vallab)
  
  expect_s3_class(result, "factor")
  expect_equal(as.character(result), c("1", "2", "3"))
})

test_that("make_factor treats negative codes as missing", {
  vallab <- tibble::tibble(
    variable = c("q1", "q1"),
    value = c("1", "2"),
    label = c("Yes", "No")
  )
  
  x <- c(1, 2, -9, -1)
  result <- make_factor(x, "q1", vallab)
  
  expect_equal(sum(is.na(result)), 2)
  expect_equal(as.character(result[1:2]), c("Yes", "No"))
})

test_that("collapse_with_drop works correctly", {
  fct <- factor(c("A", "B", "C", "D", "A", "B"))
  mapping <- list(
    "Group1" = c("A", "B"),
    "Group2" = c("C")
  )
  
  result <- collapse_with_drop(fct, mapping)
  
  expect_equal(levels(result), c("Group1", "Group2"))
  expect_false("D" %in% as.character(result))
})

test_that("collapse_with_drop returns original if no mapping", {
  fct <- factor(c("A", "B", "C", "A"))
  result <- collapse_with_drop(fct, list())
  
  expect_equal(levels(result), c("A", "B", "C"))
})

test_that("get_variable_levels extracts from vallab", {
  vallab <- tibble::tibble(
    variable = c("q1", "q1", "q1"),
    value = c("1", "2", "3"),
    label = c("Low", "Medium", "High")
  )
  
  result <- get_variable_levels("q1", vallab, data.frame())
  
  expect_equal(result, c("Low", "Medium", "High"))
})

test_that("get_variable_levels falls back to data values", {
  vallab <- tibble::tibble(
    variable = character(0),
    value = character(0),
    label = character(0)
  )
  
  data <- data.frame(q1 = c("A", "B", "C", "A"))
  result <- get_variable_levels("q1", vallab, data)
  
  expect_setequal(result, c("A", "B", "C"))
})