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
