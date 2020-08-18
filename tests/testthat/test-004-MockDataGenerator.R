library(testthat)
library(checkmate)

context("MockDataGenerator")

test_that("MockDataGenerator can generate a list of data frames", {
  # Given
  col_types <- c("integer", "string", "boolean", "numeric")
  conf <- list(
    first_df = list(
      columns = list(
        first = list(
          type = sample(col_types, 1)
        ),
        second = list(
          type = sample(col_types, 1)
        )
      )
    ),
    second_df = list(
      columns = list(
        first = list(
          type = sample(col_types, 1)
        ),
        second = list(
          type = sample(col_types, 1)
        )
      )
    )
  )

  # When
  result <- MockDataGenerator$new(conf)

  # Then
  expect_list(result$get_all_data())
  expect_true(length(result$get_all_data()) == 2)
  expect_data_frame(result$get_all_data()[[1]])
  expect_data_frame(result$get_all_data()[[2]])
})
