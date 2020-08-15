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
  # When
  result <- random_string()

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
  letter <- sample(letters, 1)
  pattern <- glue::glue("[{letter}]")

  # When
  result <- random_string(pattern = pattern)

  # Then
  expect_string(result, pattern = letter)
})

test_that("random_boolean function returns a single boolean", {
  # When
  result <- random_boolean()

  # Then
  expect_logical(result, len = 1, any.missing = FALSE)
})

test_that("random_numeric function returns a single numeric value", {
  # When
  result <- random_numeric()

  # Then
  expect_number(result)
})

test_that("random_numeric function returns a numeric value lower or equal given maximum", {
  # Given
  maximum <- runif(1)

  # When
  result <- random_numeric(max = maximum)

  # Then
  expect_number(result, upper = maximum)
})

test_that("random_numeric function returns a numeric value higher or equal given minimum", {
  # Given
  minimum <- runif(1)

  # When
  result <- random_numeric(min = minimum)

  # Then
  expect_number(result, lower = minimum)
})
