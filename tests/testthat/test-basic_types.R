library(testthat)
library(checkmate)

context("basic_types")

test_that("random_integer function returns an integer", {
  # When
  result <- random_integer()

  # Then
  checkmate::expect_int(result)
})

test_that("_random_integer function returns an integer lower or equal given maximum", {
  # Given
  maximum <- sample(1:100, 1)

  # When
  result <- random_integer(max = maximum)

  # Then
  expect_true(result <= maximum)
})

test_that("random_integer function returns an integer higher or equal given minimum", {
  # Given
  minimum <- sample(1:100, 1)

  # When
  result <- random_integer(min = minimum)

  # Then
  expect_true(result >= minimum)
})

test_that("random_string function returns a string", {
  # Given
  length <- fixtuRes::random_integer(min = 1, max = 10)

  # When
  result <- random_string(length)

  # Then
  expect_string(result)
})

test_that("random_string function resturns a string with desired length", {
   # Given
  length <- fixtuRes::random_integer(min = 1, max = 10)

  # When
  result <- random_string(length)

  # Then
  expect_true(nchar(result) == length)
})

test_that("random_string function returns a string that follows pattern", {
  # Given
  length <- fixtuRes::random_integer(min = 1, max = 10)
  letter <- sample(letters, 1)
  pattern <- glue::glue("[{letter}]")

  # When
  result <- random_string(length = length, pattern = pattern)

  # Then
  expect_string(result, pattern = letter)
})

test_that("random_boolean function returns a single boolean", {
  # When
  result <- random_boolean()

  # Then
  expect_logical(result, len = 1, any.missing = FALSE)
})
