# TODO docstring
#' Run optimization and save the results into a .csv file
#'
#' This function runs the provided optimization method and saves the results
#' such as solution, messages, execution time, etc into a .csv file.
#'
#' @param
#' @return
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
#' This function saves the dataframe to the output_file (csv). If the file
#' already exists, the data will be appended. If the data is incompatible with
#' the existing file, a new file will be created.
#'
#' @param dataframe A dataframe to be saved.
#' @param output_file The path to the csv file.
#' @return A character message indicating whether a new file was created or
#' if the data was appended to an existing file.
save_df_to_csv <- function(dataframe, output_file) {
  dir_path <- dirname(output_file)
  if (!dir.exists(dir_path)) {
    stop("The path to the output_file does not exist!")
  }

  csv_data <- load_csv_data(output_file)
  if (is.null(csv_data)) {
    # the file does not exist, create a new one
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
        new_file <- paste(substr(output_file, 1, nchar(output_file)-4),
                          "_new.csv", sep = "")
        # cannot combine the existing data with the new dataframe
        return(save_df_to_csv(dataframe, new_file))
      }
    )
  }
}


#' Load csv file
#'
#' Load csv file and return loaded data as a dataframe. If the file cannot be
#' read, return NULL.
#'
#' @param output_file The path to the csv file.
#' @return The loaded data in a dataframe or NULL.
load_csv_data <- function(output_file) {
  tryCatch(
    {
      csv_data <- read.csv(output_file, header = TRUE)
    },
    error = function(e) {
      csv_data <- NULL
    },
    warning = function(w) {
      csv_data <- NULL
    }
  )
  return(csv_data)
}


# TODO
#' Run optimization and return the results in a unified way
#'
#' Run optimization and return the results (including execution time)
#' as a dataframe with 1 row in a unified way.
#'
#' @param time_units character string. Units for execution time measurement,
#' the same format is supported as in (base) difftime function.
#' @return
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
  df <- data.frame(R_package = package, method = optimization_method
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

