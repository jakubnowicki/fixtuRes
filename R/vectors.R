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

#' Generate a random vector of desired type
#'
#' @param size integer, vector length
#' @param type "integer", "string", "boolean" or "numeric" type of vector values.
#'  If custom generator provided, should be set to "custom".
#' @param unique boolean, should the output contain only unique values. Default: FALSE.
#' @param custom_generator function or string, custom value generator.
#'  Can be a function or a string with function name. Default: NULL
#' @param ... arguments passed to function responsible for generating values.
#'  Check \code{\link{random_integer}}, \code{\link{random_string}}, \code{\link{random_boolean}}
#'  and \code{\link{random_numeric}} for details
#' @return vector of random values of chosen type
#' @export
#' @importFrom checkmate assert_choice
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

  if (type == "custom" && is.character(custom_generator)) {
    custom_generator <- get(custom_generator)
  }

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
#' @param size integer, vector length
#' @param set vector a set of values to pick from; default: NULL
#' @param set_type string if set is NULL generate a random set of type
#'  ("integer", "string", "boolean", "numeric"); default: NULL
#' @param set_size integer, number of elements in random set; default: NULL
#' @param ... additional arguments for random set generator.
#'  For details check \code{\link{random_vector}}
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

#' Wrapper that allows generating a special type vectors
#'
#' @param size integer, vector length
#' @param type type of vector, one of: "id", "distribution"
#' @param configuration list of arguments required by vector function
#'
#' @export
#'
#' @examples
#' special_vector(10, "id", list(start = 3))
special_vector <- function(size, type, configuration) {
  switch(
    type,
    id = do.call(id_vector, c(size, configuration)),
    distribution = do.call(
      distribution_vector,
      list(
        size,
        distribution_type = configuration$distribution_type,
        distribution_arguments = configuration[names(configuration) != "distribution_type"]
      )
    )
  )
}

#' Get random date vector from an interval
#'
#' @param size integer, vector length
#' @param min_date character or date, begining of the time interval to sample from
#' @param max_date character or date, ending of the time interval to sample from
#' @param format character, check \code{\link[base]{strptime}} for details
#' @param tz character, time zone name
#' @param unique boolean, should the output be unique?
#' @export
#'
#' @importFrom lubridate as_date
#' @examples
#' random_date_vector(12, "2012-12-04", "2020-10-31")
random_date_vector <- function(size, min_date, max_date, format = NULL, tz = NULL, unique = FALSE) {
  as_date(
    sample(
      as_date(min_date, format = format, tz = tz):as_date(max_date, format = format, tz = tz),
      size,
      replace = !unique
    )
  )
}
