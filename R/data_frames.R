#' Generate a random data frame from given configuration
#'
#' @param configuration list, a configuration of columns with
#'  all arguments required by vector generator passed as
#'  sublists of sublist "columns". Column can be also generated with custom function.
#'  Pass "custom_column" as column type and function (or function name) as custom_column_generator.
#'  Column generator has to accept argument size and return a vector of this size.
#'  Third option is to pass an expression that involves existing columns. This can be a simple one,
#'  or a call of an existing function.
#' @param size integer, number of rows to generate.
#' @export
#' @importFrom rlang parse_expr
#' @importFrom rlang fn_fmls_names
#' @importFrom dplyr arrange
#' @importFrom dplyr across
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
  if (is.null(configuration$columns)) {
    stop("Columns are missing in provided configuration.")
  }
  col_names <- purrr::map(configuration, ~names(.x))$columns

  simple <- purrr::discard(configuration$columns, ~.x$type == "calculated")
  calculated <- purrr::keep(configuration$columns, ~.x$type == "calculated")
  simple_columns <- purrr::map(simple, ~create_column_wrapper(.x, size = size))
  calculated_functions <- purrr::map(
    calculated,
    ~calculated_column_functions(.x$formula, columns = simple_columns)
  )
  output <- calculate_columns(simple_columns, calculated_functions)
  output <- as.data.frame(output, stringsAsFactors = FALSE)
  output <- output[, col_names]

  if (!is.null(configuration$arrange)) {
    output <- arrange(output, across(configuration$arrange))
  }

  output
}

# Wrapper that allows passing additional external arguments (e.g. size)
create_column_wrapper <- function(configuration, size) {
  args <- list(configuration = configuration, size = size)
  return(do.call(create_column, args))
}

# Create data frame column
create_column <- function(size, configuration) {
  if (configuration$type %in% c("id", "distribution")) {
    special_type <- configuration$type
    configuration$type <- "special_vector"
  }

  generator <- switch(configuration$type,
    set = do.call(
      set_vector,
      c(size = size, configuration[names(configuration) != "type"])
    ),
    custom_column = do.call(
      configuration$custom_column_generator,
      c(
        size = size,
        configuration[!(names(configuration) %in% c("type", "custom_column_generator"))]
      )
    ),
    special_vector = do.call(
      special_vector,
      list(
        type = special_type,
        size = size,
        configuration = configuration[names(configuration) != "type"]
      )
    ),
    do.call(
      random_vector,
      c(type = configuration$type, size = size, configuration[names(configuration) != "type"])
    )
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
  args <- paste(all.vars(rlang::parse_expr(string)), collapse = ", ") #nolint
  glue::glue(
    "function({args}) {{
        {string}
      }}"
  )
}

# Calculate column from formula
calculated_column_functions <- function(fun, columns) {
  if (!check_if_is_function(fun)) {
    fun <- convert_to_function(fun)
  }
  fun <- eval(rlang::parse_expr(fun))
  function_args <- rlang::fn_fmls_names(fun)
  return(list(fun = fun, args = function_args))
}

# Calculate columns
calculate_columns <- function(simple_columns, functions) {
  simple_names <- names(simple_columns)
  calculated_names <- names(functions)

  all_args <- unlist(purrr::map(functions, ~.x$args))

  if (!all(all_args %in% c(simple_names, calculated_names))) {
    stop("Caclulated columns require columns that do not exist.")
  }

  columns <- recursive_column_calculation(simple_columns, functions)

  return(columns)
}

# Recursive function for calculating columns
recursive_column_calculation <- function(simple_columns, functions) {
  functions_names <- names(functions)
  for (function_name in functions_names) {
    fun <- functions[[function_name]]
    if (all(fun$args %in% names(simple_columns))) {
      simple_columns[[function_name]] <- do.call(fun$fun, simple_columns[fun$args])
      functions[[function_name]] <- NULL
      if (length(functions) == 0) {
        break() #nolint
      } else {
        simple_columns <- recursive_column_calculation(simple_columns, functions)
      }
    }
  }
  return(simple_columns)
}
