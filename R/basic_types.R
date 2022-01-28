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


#' Get random date from an interval
#'
#' @param min_date character or date, beginning of the time interval to sample from
#' @param max_date character or date, ending of the time interval to sample from
#' @param format character, check \code{\link[base]{strptime}} for details
#' @export
#'
#' @importFrom lubridate as_date
#' @examples
#' random_date("2012-12-04", "2020-10-31")
random_date <- function(min_date, max_date, format = NULL) {
  random_date_vector(
    size = 1,
    min_date = min_date,
    max_date = max_date,
    format = format
  )
}

#' Get random time from an interval
#'
#' @param min_time character, beginning of the time interval to sample from
#' @param max_time character, ending of the time interval to sample from
#' @param resolution character, one of "seconds", "minutes", "hours", time resolution
#' @export
#'
#' @examples
#' random_time("12:23:00", "15:48:32")
random_time <- function(min_time = "00:00:00", max_time = "23:59:59", resolution = "seconds") {
  random_time_vector(
    size = 1,
    min_time = min_time,
    max_time = max_time,
    resolution = resolution
  )
}

#' Get random datetime
#'
#' @param min_date character or date, beginning of the dates interval to sample from
#' @param max_date character or date, ending of the dates interval to sample from
#' @param date_format character, check \code{\link[base]{strptime}} for details
#' @param min_time character, beginning of the time interval to sample from
#' @param max_time character, ending of the time interval to sample from
#' @param time_resolution character, one of "seconds", "minutes", "hours", time resolution
#' @param tz character, time zone to use
#' @export
#'
#' @examples
#' random_datetime("2012-12-04", "2020-10-31", min_time = "7:00:00", max_time = "17:00:00")
random_datetime <- function(min_date,
                            max_date,
                            date_format = NULL,
                            min_time = "00:00:00",
                            max_time = "23:59:59",
                            time_resolution = "seconds",
                            tz = "UTC") {
  random_datetime_vector(
    1,
    min_date = min_date,
    max_date = max_date,
    date_format = date_format,
    date_unique = FALSE,
    min_time = min_time,
    max_time = max_time,
    time_resolution = time_resolution,
    time_unique = FALSE,
    tz = tz
  )
}
