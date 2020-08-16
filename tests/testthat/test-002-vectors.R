library(testthat)
library(checkmate)

context("vectors")

test_that("random_vector function returns a vector of a selected type", {
  # When
  result_integer <- random_vector(size = random_integer(min = 1, max = 10), type = "integer")
  result_string <- random_vector(size = random_integer(min = 1, max = 10), type = "string")
  result_boolean <- random_vector(size = random_integer(min = 1, max = 10), type = "boolean")
  result_numeric <- random_vector(size = random_integer(min = 1, max = 10), type = "numeric")

  # Then
  expect_integer(result_integer)
  expect_character(result_string)
  expect_logical(result_boolean)
  expect_numeric(result_numeric)
})

test_that("random_vector function returns a vector of selected size", {
  # Given
  size_one <- random_integer(max = 20)
  size_two <- random_integer(max = 20)
  types <- c("integer", "string", "boolean", "numeric")
  type_one <- sample(types, 1)
  type_two <- sample(types, 1)

  # When
  result_one <- random_vector(size = size_one, type = type_one)
  result_two <- random_vector(size = size_two, type = type_two)

  # Then
  expect_true(length(result_one) == size_one)
  expect_true(length(result_two) == size_two)
})

test_that("set_vector function returns a vector that contains only provided values", {
  # Given
  set <- replicate(3, random_string())

  # When
  result <- set_vector(size = random_integer(min = 10, max = 20), set = set)

  # Then
  expect_true(all(result %in% set))
})
