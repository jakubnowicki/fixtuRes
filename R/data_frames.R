#' Generate a random data frame from given configuration
#'
#' @param configuration list; a configuration of columns with data frame size
#'  passed as "size" and all arguments required by vector generator passed as
#'  sublists of sublist "columns"
#' @export
#' @import purrr
#' @return data.frame
#'
#' @examples
#' conf <- list(
#'  size = 10,
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
#' random_data_frame(conf)
random_data_frame <- function(configuration) {
  size <- configuration$size
  columns <- purrr::map(configuration$columns, ~create_column_wrapper(.x, size = size))

  return(as.data.frame(columns, stringsAsFactors = FALSE))
}

# Wrapper that allows passing additional external arguments (eg. size)
create_column_wrapper <- function(configuration, ...) {
  args <- c(configuration, list(...))
  return(do.call(create_column, args))
}

# Create data frame column
create_column <- function(size, type, ...) {
  generator <- switch(type,
    set = do.call(set_vector, list(size = size, ...)),
    do.call(random_vector, list(type = type, size = size, ...))
  )

  return(generator)
}
