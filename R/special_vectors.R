#' id vector with sequence of integers
#'
#' @param size integer, size of the output vector
#' @param start integer, value of the first element
#'
#' @export
#'
#' @examples
#' id_vector(10, 2)
id_vector <- function(size, start = 1) {
  start:(start + size - 1)
}

#' vector of values that follow specified distribution
#'
#' @param size integer, size of the output vector
#' @param distribution_type character, type of distribution
#' @param ... arguments required by the distribution function
#'
#' @export
#'
#' @examples
#' distribution_vector(10, "normal", mean = 2, sd = 0.5)
distribution_vector <- function(size, distribution_type, ...) {
  distribution_function_name <- convert_distribution_name_to_function(distribution_type)

  args <- list(n = size, ...)

  do.call(distribution_function_name, args)
}

convert_distribution_name_to_function <- function(distribution_name) {
  dist_name_lower <- tolower(distribution_name)
  distributions <- yaml::read_yaml(system.file("distributions.yaml", package = "fixtuRes"))
  function_name <- names(purrr::keep(distributions, ~dist_name_lower %in% .x))

  if (length(function_name) == 1) {
    return(function_name)
  } else {
    return(distribution_name)
  }
}
