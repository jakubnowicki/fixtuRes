#' MockDataGenerator R6 Class
#'
#' Object that stores mock data configurations and generated datasets
#' @import purrr
#' @import R6
#' @export
MockDataGenerator <- R6::R6Class(
  classname = "MockDataGenerator",
  private = list(
    generators = list(),
    data = list()
  ),
  public = list(
    #' @description
    #' Create a new MockDataGenerator object
    #' @param configuration list; list with datasets configurations.
    #'  Check \code{random_data_frame} for configuration details.
    #' @return A new MockDataGenerator object
    initialize = function(configuration) {
      private$generators <- purrr::map(configuration, create_data_generator)
    },
    #' @description
    #' Generate and save a dataset
    #' @param data_name string; name of data set to generate
    #' @param size integer; number of rows in generated data frame
    generate_data = function(data_name, size) {
      private$data[[data_name]] <- private$generators[[data_name]](size)
    },
    #' @description
    #' Get a dataset (if does not exist, generate it)
    #' @param data_name string; data set name to retrieve
    #' @param size integer; size of dataset (if not generated before)
    #' @return mock dataset
    get_data = function(data_name, size = NULL) {
      if (is.null(private$data[[data_name]])) {
        self$generate_data(data_name, size = size)
      }

      return(private$data[[data_name]])
    }
  )
)
