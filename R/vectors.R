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
#' @param type "integer", "string", "boolean", "date", "time",
#'  "datetime" or "numeric" type of vector values.
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
    choices = c(
      "integer",
      "string",
      "boolean",
      "numeric",
      "custom",
      "date",
      "time",
      "datetime"
    )
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
      custom = do.call(custom_generator, args),
      date = do.call(random_date_vector, args),
      time = do.call(random_time_vector, args),
      datetime = do.call(random_datetime_vector, args)
    )
  }

  if (type %in% c("date", "time", "datetime")) {
    args <- c(args, size = size)
    if (type != "datetime") {
      args <- c(args, unique = unique)
    }
    output <- do.call(generator, args)
    return(output)
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
#' @param min_date character or date, beginning of the time interval to sample from
#' @param max_date character or date, ending of the time interval to sample from
#' @param format character, check \code{\link[base]{strptime}} for details
#' @param unique boolean, should the output be unique?
#' @export
#'
#' @importFrom lubridate as_date
#' @examples
#' random_date_vector(12, "2012-12-04", "2020-10-31")
random_date_vector <- function(size, min_date, max_date, format = NULL, unique = FALSE) {
  as_date(
    sample(
      as_date(min_date, format = format, tz = NULL):as_date(max_date, format = format, tz = NULL),
      size,
      replace = !unique
    )
  )
}

#' Get random time vector from an interval
#'
#' @param size integer, vector length
#' @param min_time character, beginning of the time interval to sample from
#' @param max_time character, ending of the time interval to sample from
#' @param resolution character, one of "seconds", "minutes", "hours", time resolution
#' @param unique boolean, should the output be unique?
#' @export
#'
#' @importFrom lubridate hms hm hours minutes seconds seconds_to_period period_to_seconds
#' @importFrom purrr `%>%` map reduce
#' @examples
#' random_time_vector(12, "12:23:00", "15:48:32")
random_time_vector <- function(size,
                               min_time = "00:00:00",
                               max_time = "23:59:59",
                               resolution = "seconds",
                               unique = FALSE) {
  conversion_function <- switch(
    resolution,
    seconds = hms,
    minutes = hm,
    hours = hours
  )

  resolution_coefficient <- switch(
    resolution,
    seconds = 1,
    minutes = 60,
    hours = 3600
  )

  differences_conversion_function <- switch(
    resolution,
    seconds = identity,
    minutes = minutes,
    hours = hours
  )

  available_period <- period_to_seconds(
    conversion_function(max_time) - conversion_function(min_time)
  ) / resolution_coefficient

  time_differences <- sample(1:as.numeric(available_period), size, replace = !unique)

  map(time_differences, ~ conversion_function(min_time) + seconds_to_period(differences_conversion_function(.x))) %>%
    reduce(c)
}

#' Get random datetime vector
#'
#' @param size integer, vector length
#' @param min_date character or date, beginning of the dates interval to sample from
#' @param max_date character or date, ending of the dates interval to sample from
#' @param date_format character, check \code{\link[base]{strptime}} for details
#' @param date_unique boolean, should the date part of the output  be unique?
#' @param min_time character, beginning of the time interval to sample from
#' @param max_time character, ending of the time interval to sample from
#' @param time_resolution character, one of "seconds", "minutes", "hours", time resolution
#' @param time_unique boolean, should the time part of the output be unique?
#' @param tz character, time zone to use
#' @export
#'
#' @importFrom lubridate force_tz
#' @examples
#' random_datetime_vector(12, "2012-12-04", "2020-10-31", min_time = "7:00:00", max_time = "17:00:00")
random_datetime_vector <- function(size,
                                   min_date,
                                   max_date,
                                   date_format = NULL,
                                   date_unique = FALSE,
                                   min_time = "00:00:00",
                                   max_time = "23:59:59",
                                   time_resolution = "seconds",
                                   time_unique = FALSE,
                                   tz = "UTC") {
  dates <- random_date_vector(
    size,
    min_date = min_date,
    max_date = max_date,
    format = date_format,
    unique = date_unique
  )

  times <- random_time_vector(
    size,
    min_time = min_time,
    max_time = max_time,
    resolution = time_resolution,
    unique = time_unique
  )

  return(force_tz(dates + times, tzone = tz))
}
