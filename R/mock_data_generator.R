#' MockDataGenerator
#'
#' @description Object that stores mock data configurations and generated datasets
#' @import purrr
#' @import R6
#' @import glue
#' @import yaml
#' @importFrom checkmate test_string test_function test_int
#' @export
MockDataGenerator <- R6::R6Class( #nolint
  classname = "MockDataGenerator",
  private = list(
    generators = list(),
    default_sizes = list(),
    data = list(),
    get_default_size = function(configuration) {
      if (is.null(configuration$default_size)) {
        return(function() random_integer(min = 5, max = 50))
      }

      if (test_int(configuration$default_size)) {
        return(configuration$default_size)
      }

      if (isTRUE(configuration$default_size$static)) {
        return(do.call(random_integer, configuration$default_size$arguments))
      } else {
        return(function() do.call(random_integer, configuration$default_size$arguments))
      }
    },
    generate_data = function(data_name, size) {
      private$data[[data_name]] <- private$generators[[data_name]](size)
    }
  ),
  public = list(
    #' @description
    #' Create a new MockDataGenerator object
    #' @param configuration list or path to YAML file with datasets configurations.
    #'  Check
    #'  \href{https://github.com/jakubnowicki/fixtuRes/blob/master/vignettes/configuration.Rmd}{configuration}
    #'  for details.
    #'  For a sample YAML check
    #'  \href{https://github.com/jakubnowicki/fixtuRes/tree/master/examples}{examples}.
    #' @return A new MockDataGenerator object
    initialize = function(configuration) {
      if (test_string(configuration)) {
        configuration <- yaml::read_yaml(configuration)
      }
      private$generators <- purrr::map(configuration, create_data_generator)
      private$default_sizes <- purrr::map(configuration, private$get_default_size)
    },
    #' @description
    #' Get a dataset (if does not exist, generate it)
    #' @param data_name string, data set name to retrieve
    #' @param size integer, size of dataset (if provided, will refresh dataset)
    #' @param refresh boolean, refresh existing data?
    #' @return mock dataset
    get_data = function(data_name, size = NULL, refresh = FALSE) {
      if (is.null(private$data[[data_name]]) || isTRUE(refresh) || !is.null(size)) {
        size <- if (is.null(size)) eval(private$default_sizes[[data_name]]) else size
        if (test_function(size)) size <- size()
        private$generate_data(data_name, size = size)
      }

      return(private$data[[data_name]])
    },
    #' @description
    #' Get all datasets
    #' @param refresh boolean, refresh existing data?
    #' @param sizes integer, or vector of integers with data sizes
    #' @return list with all datasets
    get_all_data = function(refresh = FALSE, sizes = NULL) {
      data_names <- names(private$generators)
      if (length(sizes) > 1) {
        if (length(sizes) != length(data_names)) {
          stop(
            glue::glue(
              "Wrong number of sizes. Provide a single integer or a vector of exactly {length(data_names)} integers."
              )
            )
        }
        data <- purrr::map2(data_names, sizes, self$get_data, refresh = refresh)
      } else {
        data <- purrr::map(data_names, self$get_data, refresh = refresh, size = sizes)
      }
      names(data) <- data_names
      return(data)
    }
  )
)
