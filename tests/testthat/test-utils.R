# tests/testthat/test-utils.R
# Unit tests for utility functions

library(testthat)

test_that("%||% returns default for NULL", {
  expect_equal(NULL %||% "default", "default")
})

test_that("%||% returns default for empty string", {
  expect_equal("" %||% "default", "default")
})

test_that("%||% returns value for non-empty string", {
  expect_equal("value" %||% "default", "value")
})

test_that("%||% returns value for non-NULL", {
  expect_equal(5 %||% 10, 5)
})

test_that("create_safe_filename sanitizes text", {
  result <- create_safe_filename("Question: What is 2+2?", "test", "csv")
  expect_match(result, "^test_Question__What_is_2_2_\\.csv$")
})

test_that("create_safe_filename handles special characters", {
  result <- create_safe_filename("Test@#$%", "prefix", "txt")
  expect_match(result, "^prefix_Test_+\\.txt$")
})

test_that("collect_group_mappings extracts valid mappings", {
  input <- list(
    var_group_name_1 = "Group A",
    var_group_levels_1 = c("Level1", "Level2"),
    var_group_name_2 = "Group B",
    var_group_levels_2 = c("Level3"),
    var_group_name_3 = "",
    var_group_levels_3 = c("Level4"),
    var_group_name_4 = "Group C",
    var_group_levels_4 = NULL
  )
  
  result <- collect_group_mappings(input, "var_group_name_", "var_group_levels_", 4)
  
  expect_equal(length(result), 2)
  expect_equal(result[["Group A"]], c("Level1", "Level2"))
  expect_equal(result[["Group B"]], "Level3")
  expect_null(result[["Group C"]])
})

test_that("collect_group_mappings returns empty list when no valid mappings", {
  input <- list(
    var_group_name_1 = "",
    var_group_levels_1 = c("Level1")
  )
  
  result <- collect_group_mappings(input, "var_group_name_", "var_group_levels_", 1)
  
  expect_equal(length(result), 0)
})