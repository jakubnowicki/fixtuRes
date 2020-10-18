library(testthat)
library(checkmate)
library(fixtuRes)

context("configuration examples")

source("additional_functions.R")

test_that("basic_example configuration returns a valid data frame", {
  # Given
  mock_generator <- MockDataGenerator$new("basic_example.yaml")

  # Then
  expect_data_frame(mock_generator$get_all_data()[[1]])
})

test_that("built_in_columns configuration returns a valid data frame", {
  # Given
  mock_generator <- MockDataGenerator$new("built_in_columns.yaml")

  # Then
  expect_data_frame(mock_generator$get_all_data()[[1]])
})

test_that("calculated_columns configuration returns a valid data frame", {
  # Given
  mock_generator <- MockDataGenerator$new("calculated_columns.yaml")

  # Then
  expect_data_frame(mock_generator$get_all_data()[[1]])
})

test_that("custom_columns configuration returns a valid data frame", {
  # Given
  mock_generator <- MockDataGenerator$new("custom_columns.yaml")

  # Then
  expect_data_frame(mock_generator$get_all_data()[[1]])
})

test_that("default_size_examples configuration returns a valid data frame", {
  # Given
  print(getwd)
  mock_generator <- MockDataGenerator$new("default_size_examples.yaml")

  # Then
  expect_data_frame(mock_generator$get_all_data()[[1]])
  expect_data_frame(mock_generator$get_all_data()[[2]])
  expect_data_frame(mock_generator$get_all_data()[[3]])
  expect_data_frame(mock_generator$get_all_data()[[4]])
})

test_that("special_types configuration returns a valid data frame", {
  # Given
  mock_generator <- MockDataGenerator$new("special_types.yaml")

  # Then
  expect_data_frame(mock_generator$get_all_data()[[1]])
})
