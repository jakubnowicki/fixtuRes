#' Generate random integer
#'
#' @param min integer minimum
#' @param max integer maximum
#' @return random integer
#' @export
#'
#' @examples
#' integer(min = 2, max = 10)
integer <- function(min = 0, max = 999999) {
  sample(min:max, 1)
}

#' Generate random string
#'
#' @param length integer string length
#' @param pattern string pattern for string to follow
#' @export
#' @import stringi
#'
#' @examples
string <- function(length, pattern = "[A-Za-z0-9]") {
  stringi::stri_rand_strings(1, length = length, pattern = pattern)
}
