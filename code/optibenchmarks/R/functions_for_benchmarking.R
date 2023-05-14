# R script for optimization benchmarks functions
# Author: Jana Viktoria Kovacikova
# Date: May 14, 2023


#' Run optimization and save the results into a .csv file
#'
#' This function runs the provided optimization method and saves the results
#' such as solution, messages, execution time, etc into a .csv file.
#'
#' @param package The name of the package that the optimization method
#' is supposed to be loaded from.
#' @param optimization_method The name of the optimization method.
#' @param parameters_for_optimization_method A list containing all parameters
#' that would normally be passed to the provided optimization method.
#' @param output_file A path where the results should be stored. The path must
#' contain the name of the csv file (for example "path/filename.csv").
#' @param time_units A character string. Units for execution time measurement,
#' the same format is supported as in (base) difftime function.
#' @examples
#' Rosenbrock <- function(x) {(1 - x[1])^2 + 100 * (x[2] - x[1]^2)^2}
#' run_and_save(package = "stats", optimization_method = "optim",
#'              parameters_for_optimization_method = list(
#'                par = c(2, -1), fn = Rosenbrock, method = "L-BFGS-B",
#'                lower = c(-5, -5), upper = c(5, 5),
#'                control=list(maxit=1000)),
#'              output_file = "export.csv")
#' @export
run_and_save <- function(package, optimization_method,
                         parameters_for_optimization_method,
                         output_file,
                         time_units="secs") {

  result <- get_results(package, optimization_method,
                        parameters_for_optimization_method, time_units)

  # save the result
  save_df_to_csv(result, output_file)
}


#' Save a dataframe to csv safely
#'
#' This function saves the dataframe to the output_file (csv). If the file does
#' not exist or can't be read or is incompatible with the new data, a new file
#' will be created. Otherwise the data will be appended.
#'
#' @param dataframe A dataframe containing the data to be saved.
#' @param output_file A path where the data should be stored. The path must
#' contain the name of the csv file (for example "path/filename.csv").
#' @return A character string. A message indicating whether a new file was
#' created or whether the data was appended to an existing file.
save_df_to_csv <- function(dataframe, output_file) {
  dir_path <- dirname(output_file)
  if (!dir.exists(dir_path)) {
    stop("The path to the output_file does not exist!")
  }

  csv_data <- load_csv_data(output_file)

  if (is.null(csv_data)) {
    # the file does not exist or can't be opened, so create a new one
    write.csv(dataframe, file = output_file, row.names = FALSE)
    return(sprintf("Creating a new file %s", output_file))
  }
  else {
    tryCatch(
      {
        combined_dataframe <- rbind(dataframe, csv_data)
        # append the data
        write.csv(combined_dataframe, file = output_file, row.names = FALSE)
        return(sprintf("Appending to the file %s", output_file))
      },
      error = function(e) {
        # cannot combine the existing data with the new dataframe
        new_file <- paste(substr(output_file, 1, nchar(output_file)-4),
                          "_new.csv", sep = "")
        return(save_df_to_csv(dataframe, new_file))
      }
    )
  }
}


#' Load the csv file
#'
#' Load the csv file from path and return loaded data as a dataframe with header.
#' If the file cannot be read, return NULL.
#'
#' @param path_to_csv_file The path to the csv file.
#' @return The loaded data as a dataframe or NULL.
load_csv_data <- function(path_to_csv_file) {
  csv_data <- NULL
  if (file.exists(path_to_csv_file)) {
    tryCatch(
      {csv_data <- read.csv(path_to_csv_file, header = TRUE)},
      error = function(e) {},
      warning = function(w) {}
    )
  }
  return(csv_data)
}


# TODO
#' Run optimization and return the results in a unified way
#'
#' Run optimization and return the results (including execution time)
#' as a dataframe with 1 row in a unified way.
#'
#' @param package The name of the package that the optimization method
#' is supposed to be loaded from.
#' @param optimization_method The name of the optimization method.
#' @param parameters_for_optimization_method A list containing all parameters
#' that would normally be passed to the provided optimization method.
#' @param time_units character string. Units for execution time measurement,
#' the same format is supported as in (base) difftime function.
#' @return A dataframe containing the data about the optimization run and
#' the optimization results.
#' @examples
#' Rosenbrock <- function(x) {(1 - x[1])^2 + 100 * (x[2] - x[1]^2)^2}
#' get_results(package = "stats", optimization_method = "optim",
#'             parameters_for_optimization_method = list(
#'               par = c(2, -1), fn = Rosenbrock, method = "L-BFGS-B",
#'               lower = c(-5, -5), upper = c(5, 5),
#'               control=list(maxit=1000)))
#' @export
get_results <- function(package, optimization_method,
                        parameters_for_optimization_method,
                        time_units = "secs") {

  # load the package
  tryCatch(
    {library(package, character.only = TRUE)},
    error = function(e) {
      message <- sprintf(
        "There was an error loading package '%s'. Please make sure the package exists. %s",
        paste("", package), e)
      # print the error message
      stop(message)
    }
  )

  # run the optimization
  start_time <- Sys.time()
  result <- do.call(optimization_method, parameters_for_optimization_method)
  end_time <- Sys.time()
  execution_time <- difftime(end_time, start_time, units = time_units)

  # prepare the data

  # fill the dataframe
  df <- data.frame(R_package = package, method = optimization_method,
                   method2 = optimization_method
                   # objective_function = c(objective_function)
                   # starting_point = c(starting_point),
                   # lower = c(lower), upper = c(upper),
                   # all_opti_params = paste(
                   #   named_list_to_str(parameters_for_optimization_method)),
                   # solution = c(solution),
                   # optimal_value = c(optimal_value),
                   # iterations = c(iterations),
                   # convergence = c(convergence),
                   # message = c(message),
                   # execution_time = c(execution_time),
                   # units = c(units)
  )
  return(df)
}


Rosenbrock <- function(x) {
  (1 - x[1])^2 + 100 * (x[2] - x[1]^2)^2
}
get_results(package = "stats", optimization_method = "optim",
            parameters_for_optimization_method = list(
              par = c(2, -1), fn = Rosenbrock, method = "L-BFGS-B",
              lower = c(-5, -5), upper = c(5, 5),
              control=list(maxit=1000)))
run_and_save(package = "stats", optimization_method = "optim",
             parameters_for_optimization_method = list(
              par = c(2, -1), fn = Rosenbrock, method = "L-BFGS-B",
              lower = c(-5, -5), upper = c(5, 5),
              control=list(maxit=1000)),
             output_file = "/home/viki/Dokumenty/vikine_skolske/bakalarka_final/export.csv")

