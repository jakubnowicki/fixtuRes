return_sample_paste <- function(vector_of_values) {
  values <- sample(vector_of_values, 2)
  paste(values, collapse = "_")
}

return_repeated_value <- function(size, value) {
  rep(value, times = size)
}

check_column <- function(column) {
  purrr::map_lgl(column, ~.x >= 10)
}
