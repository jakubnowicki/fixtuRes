# Create a vector of unique values
replicate_unique <- function(size, generator, args) {
  output <- NULL
  while (length(output) < size) {
    new_value <- do.call(generator, args)
    if (!(new_value %in% output)) {
      output <- c(output, new_value)
    }
  }

  return(output)
}

#' Generate random vector of desired type
#'
#' @param size integer vector length
#' @param type "integer", "string", "boolean" or "numeric" type of vector values.
#'  If custom generator provided, should be set to "custom".
#' @param unique boolean should the output contain only unique values
#' @param custom_generator function; custom value generator. Default: NULL
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
#'
#' # custom generator
#' custom_generator <- function() sample(c("A", "B"), 1)
#' random_vector(3, type = "custom", custom_generator = custom_generator)
random_vector <- function(size, type, custom_generator = NULL, unique = FALSE, ...) {
  checkmate::assert_choice(
    type,
    choices = c("integer", "string", "boolean", "numeric", "custom")
  )

  args <- list(type, ...)

  generator <- function(type, ...) {
    args <- list(...)
    switch(
      type,
      integer = do.call(random_integer, args),
      string = do.call(random_string, args),
      boolean = random_boolean(),
      numeric = do.call(random_numeric, args),
      custom = do.call(custom_generator, args)
    )
  }

  if (isTRUE(unique)) {
    output <- replicate_unique(size, generator, args)
  } else {
    output <- replicate(size, do.call(generator, args))
  }

  return(output)
}

#' Generate a vector of a values from a set
#'
#' @param size integer vector length
#' @param set vector a set of values to pick from; default: NULL
#' @param set_type string if set is NULL generate a random set of type
#'  ("integer", "string", "boolean", "numeric"); default: NULL
#' @param set_size integer number of elements in random set; default: NULL
#' @param ... additional arguments for random set generator.
#'  For details check \code{random_vector}
#' @note When using a random set, be aware, that set has to be unique,
#'  thus if arguments passed to generator do not allow this, the function
#'  can end up in an infinite loop.
#' @export
#'
#' @examples
#' set_vector(10, set = c("a", "b", "c"))
#' set_vector(size = 5, set_type = "string", set_size = 3)
set_vector <- function(size, set = NULL, set_type = NULL, set_size = NULL, ...) {
  if (is.null(set) && is.null(set_type)) {
    stop("Provide following arguments: set or type and set size.")
  }

  if (is.null(set)) {
    if (is.null(set_size)) stop("Provide set_size.")
    set <- random_vector(size = set_size, type = set_type, unique = TRUE, ...)
  }

  return(replicate(size, random_from_set(set)))
}
