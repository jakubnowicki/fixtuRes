library(checkmate)

context("basic_types")

test_that("integer function returns an integer", {
  # When
  result <- integer()

  # Then
  expect_int(result)
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
