#' Generate random integer
#'
#' @param min integer, minimum
#' @param max integer, maximum
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
#' @param length integer or NULL (default), output string length.
#'  If NULL, length will be random
#' @param min_length integer, minimum length if length is random.
#'  Default: 1.
#' @param max_length integer, maximum length if length is random.
#'  Default: 15.
#' @param pattern string, pattern for string to follow.
#'  Check \code{\link[stringi]{stringi-search-charclass}} for details.
#' @return random string
#' @export
#' @importFrom stringi stri_rand_strings
#'
#' @examples
#' random_string(length = 5)
random_string <- function(length = NULL, min_length = 1, max_length = 15, pattern = "[A-Za-z0-9]") {
  if (is.null(length)) {
    length <- random_integer(min = min_length, max = max_length)
  }

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
#' @param min numeric, minimum
#' @param max numeric, maximum
#' @return random numeric
#' @export
#' @importFrom stats runif
#'
#' @examples
#' random_numeric(min = 1.5, max = 4.45)
random_numeric <- function(min = 0, max = 999999) {
  runif(1, min = min, max = max)
}

#' Choose random element from set
#'
#' @param set vector, set of values to choose from
#' @return a single element from a given set
#' @export
#'
#' @examples
#' random_from_set(c("a", "b", "c"))
random_from_set <- function(set) {
  sample(set, 1)
}
