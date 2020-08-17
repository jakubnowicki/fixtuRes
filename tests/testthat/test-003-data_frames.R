library(testthat)
library(checkmate)

context("data frames")

test_that("random_data_frame function returns a data frame", {
  # Given
  types <- c("integer", "string", "boolean", "numeric")
  size <- random_integer(min = 1, max = 20)
  conf <- list(
    columns = list(
      first_column = list(
        type = sample(types, 1)
      ),
      second_column = list(
        type = sample(types, 1)
      )
    )
  )

  # When
  result <- random_data_frame(conf, size)

  # Then
  expect_data_frame(result)
})
