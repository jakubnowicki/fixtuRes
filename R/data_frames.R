#' Generate a random data frame from given configuration
#'
#' @param configuration list; a configuration of columns with data frame size
#'  passed as "size" and all arguments required by vector generator passed as
#'  sublists of sublist "columns". Column can be also generated with custom function.
#'  Pass "custom_column" as column type and function (or function name) as custom_column_generator.
#'  Column generator has to accept argument size and return a vector of this size.
#'  Third option is to pass an expression that involves existing columns. This can be a simple one,
#'  or a call of an existing function.
#' @param size integer; number of rows to generate.
#' @export
#' @importFrom rlang parse_expr
#' @importFrom rlang fn_fmls_names
#' @import purrr
#' @import glue
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
#'    ),
#'    third_column = list(
#'      type = "calculated",
#'      formula = "second_column * 2"
#'    )
#'  )
#' )
#'
#' random_data_frame(conf, size = 10)
random_data_frame <- function(configuration, size) {
  simple <- purrr::discard(configuration$columns, ~.x$type == "calculated")
  calculated <- purrr::keep(configuration$columns, ~.x$type == "calculated")
  simple_columns <- purrr::map(simple, ~create_column_wrapper(.x, size = size))
  calculated_columns <- purrr::map(calculated, ~calculated_column(.x$formula, columns = simple_columns))

  as.data.frame(c(simple_columns, calculated_columns), stringsAsFactors = FALSE)
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

# Check if string is a function
check_if_is_function <- function(string) {
  grepl(pattern = "function", unlist(strsplit(string, " "))[1])
}

# convert string to a function
convert_to_function <- function(string) {
  args <- paste(all.vars(rlang::parse_expr(string)), collapse = ", ")
  glue::glue(
    "function({args}) {{
        {string}
      }}"
  )
}

# Calculate column from formula
calculated_column <- function(fun, columns) {
  if (!check_if_is_function(fun)) {
    fun <- convert_to_function(fun)
  }
  fun <- eval(rlang::parse_expr(fun))
  function_args <- rlang::fn_fmls_names(fun)
  args <- columns[function_args]
  do.call(fun, args)
}
