#' Generate a random data frame from given configuration
#'
#' @param configuration list; a configuration of columns with data frame size
#'  passed as "size" and all arguments required by vector generator passed as
#'  sublists of sublist "columns". Column can be also generated with custom function.
#'  Pass "custom_column" as column type and function (or function name) as custom_column_generator.
#'  Column generator has to accept argument size and return a vector of this size.
#' @param size integer; number of rows to generate.
#' @export
#' @import purrr
#' @return data.frame
#'
#' @examples
#' conf <- list(
#'  columns = list(
#'    first_column = list(
#'      type = "string",
#'      length = 3
#'    ),
#'    second_column = list(
#'      type = "integer",
#'      max = 10
#'    )
#'  )
#' )
#'
#' random_data_frame(conf, size = 10)
random_data_frame <- function(configuration, size) {
  columns <- purrr::map(configuration$columns, ~create_column_wrapper(.x, size = size))

  return(as.data.frame(columns, stringsAsFactors = FALSE))
}

# Wrapper that allows passing additional external arguments (eg. size)
create_column_wrapper <- function(configuration, ...) {
  args <- c(configuration, list(...))
  return(do.call(create_column, args))
}

# Create data frame column
create_column <- function(size, type, custom_column_generator = NULL, ...) {
  if (type == "custom_column" && is.character(custom_column_generator)) {
    custom_column_generator <- get(custom_column_generator)
  }

  generator <- switch(type,
    set = do.call(set_vector, list(size = size, ...)),
    custom_column = do.call(custom_column_generator, list(size = size, ...)),
    do.call(random_vector, list(type = type, size = size, ...))
  )

  return(generator)
}

# Constructor for data frame generation function
create_data_generator <- function(configuration) {
  function(size) {
    random_data_frame(configuration, size)
  }
}
