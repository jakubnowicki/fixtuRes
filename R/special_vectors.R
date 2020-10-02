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
