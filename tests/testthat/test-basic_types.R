library(testthat)
library(checkmate)

context("basic_types")

test_that("integer function returns an integer", {
  # When
  result <- integer()

  # Then
  checkmate::expect_int(result)
})

test_that("integer function returns an integer lower or equal given maximum", {
  # Given
  maximum <- sample(1:100, 1)

  # When
  result <- integer(max = maximum)

  # Then
  expect_true(result <= maximum)
})

test_that("integer function returns an integer higher or equal given minimum", {
  # Given
  minimum <- sample(1:100, 1)

  # When
  result <- integer(min = minimum)

  # Then
  expect_true(result >= minimum)
})

test_that("string function returns a string", {
  # Given
  length <- fixtuRes::integer(min = 1, max = 10)

  # When
  result <- string(length)

  # Then
  expect_string(result)
})

test_that("string function resturns a string with desired length", {
   # Given
  length <- fixtuRes::integer(min = 1, max = 10)

  # When
  result <- string(length)

  # Then
  expect_true(nchar(result) == length)
})

test_that("string function returns a string that follows pattern", {
  # Given
  length <- fixtuRes::integer(min = 1, max = 10)
  letter <- sample(letters, 1)
  pattern <- glue::glue("[{letter}]")

  # When
  result <- string(length = length, pattern = pattern)

  # Then
  expect_string(result, pattern = letter)
})
