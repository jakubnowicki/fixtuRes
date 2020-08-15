#' Generate random vector of desired type
#'
#' @param size integer vector length
#' @param type "integer", "string", "boolean" or "numeric" type of vector values
#' @param ... arguments passed to function responsible for generating values.
#'  Check \code{random_integer}, \code{random_string}, \code{random_boolean} and
#'  \code{random_numeric} for details
#' @return vector of random values of chosen type
#' @export
#' @import checkmate
#'
#' @examples
#' random_vector(5, "boolean")
#' random_vector(10, "numeric", min = 1.5, max = 5)
#' random_vector(4, "string", length = 4, pattern = "[ACGT]")
#' random_vector(2, "integer", max = 10)
random_vector <- function(size, type, ...) {
  checkmate::assert_choice(
    type,
    choices = c("integer", "string", "boolean", "numeric")
  )

  args <- list(type, ...)

  generator <- function(type, ...) {
    args <- list(...)
    switch(
      type,
      integer = do.call(random_integer, args),
      string = do.call(random_string, args),
      boolean = random_boolean(),
      numeric = do.call(random_numeric, args)
    )
  }

  replicate(size, do.call(generator, args))
}
