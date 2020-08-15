#' Generate random integer
#'
#' @param min integer minimum
#' @param max integer maximum
#' @return random integer
#' @export
#'
#' @examples
#' random_integer(min = 2, max = 10)
random_integer <- function(min = 0, max = 999999) {
  sample(min:max, 1)
}

#' Generate random string
#'
#' @param length integer string length
#' @param pattern string pattern for string to follow
#' @return random string
#' @export
#' @import stringi
#'
#' @examples
#' random_string(length = 5)
random_string <- function(length, pattern = "[A-Za-z0-9]") {
  stringi::stri_rand_strings(1, length = length, pattern = pattern)
}

#' Generate random boolean
#'
#' @return random boolean
#' @export
#'
#' @examples
#' random_boolean()
random_boolean <- function() {
  sample(c(TRUE, FALSE), 1)
}

#' Generate random numeric
#'
#' @param min numeric minimum
#' @param max numeric maximum
#' @return random numeric
#' @export
#'
#' @examples
#' random_numeric(min = 1.5, max = 4.45)
random_numeric <- function(min = 0, max = 999999) {
  runif(1, min = min, max = max)
}
