# R script for benchmarking optimization methods in R
# Author: Jana Viktoria Kovacikova
# Date: May 15, 2023


source("functions_for_benchmarking.R")
source("optimization_problems.R")
library("smoof")


# this is an example of how we benchmarked the optimization methods,
# it is rather a template, not a complete script
# (uncomment the problem and set the dimension at the beginning,
# adjust the settings of the optimization methods directly in the loop)


# Rosenbrock
# dimension <- 50
# Rosenbrock <- makeRosenbrockFunction(dimensions = dimension)
# file_path <- sprintf("/home/viki/Dokumenty/vikine_skolske/bakalarka_final/Metaheuristics-in-R/results/Rosenbrock%dD_.csv",
#                      dimension)
# set the lower and upper bound
# lower_bound <- rep(-5, dimension)
# upper_bound <- rep(5, dimension)
# generate 20 starting points (run for each time_cluster)
# starting_points <- list()
# for (i in 1:20) {
#   starting_points <- c(starting_points,
#                        list(runif(n = dimension, min = lower_bound,
#                                   max = upper_bound)))
# }


# # Lall (Problem A)
# # set size N of the experiment
# N <- 96
# lower_bound <- rep(-1, 2*N)
# upper_bound <- rep(1, 2*N)
# file_path <- sprintf("/home/viki/Dokumenty/vikine_skolske/bakalarka_final/Metaheuristics-in-R/results/LallN%d_.csv",
#                      N)
# # generate 20 starting points
# starting_points <- list()
# for (i in 1:20) {
#   new_starting_point <- 2*runif(2*N) - 1
#   starting_points <- c(starting_points,
#                        list(new_starting_point))
# }


# Duarte (Problem B)
# set size N of the experiment
N <- 200
lower_bound <- rep(-1, 3*N)
upper_bound <- rep(1, 3*N)
# set the path where results are supposed to be stored
file_path <- sprintf("/home/results/DuarteN%d_11.csv", N)
# generate 20 starting points
starting_points <- list()
for (i in 1:20) {
  new_starting_point <- 2*runif(3*N) - 1
  starting_points <- c(starting_points, list(new_starting_point))
}


# benchmark all opti methods using the same 20 starting_points
for (starting_point in starting_points) {
  # the desired execution time time_cluster is to be set,
  # as well as number of iterations/evaluations in each method
  time_cluster <- 0.01
  df <- data.frame()

  # optim L-BFGS-B
  opti_params_lbfgs <- list(par = starting_point, fn = Duarte,
                            method = "L-BFGS-B", lower = lower_bound,
                            upper = upper_bound,
                            control = list(maxit = 1, factr = 0))
  result <- get_results(package = "stats", optimization_method = "optim",
                        parameters_for_optimization_method = opti_params_lbfgs,
                        time_units = "secs", objective_name = "Duarte",
                        minimalistic_results = TRUE)
  result <- cbind(result, time_cluster)
  # df <- result

  # ga
  opti_params_ga <- list(type=c("real-valued"), fitness = minus_Duarte,
                         lower = lower_bound, upper = upper_bound,
                         maxiter = 10)
  result <- get_results(package = "GA", optimization_method = "ga",
                        parameters_for_optimization_method = opti_params_ga,
                        time_units = "secs", objective_name = "Duarte",
                        minimalistic_results = TRUE)
  result <- cbind(result, time_cluster)
  df <- rbind(df, result)

  # DEoptim
  opti_params_DEoptim <- list(fn = Duarte, lower = lower_bound,
                              upper = upper_bound,
                              control = list(itermax = 1, reltol = 0))
  result <- get_results(package = "DEoptim", optimization_method = "DEoptim",
                        parameters_for_optimization_method = opti_params_DEoptim,
                        time_units = "secs", objective_name = "Duarte",
                        minimalistic_results = TRUE)
  result <- cbind(result, time_cluster)
  df <- rbind(df, result)

  # psoptim
  opti_params_psoptim <- list(par = starting_point, fn = Duarte,
                              lower = lower_bound, upper = upper_bound,
                              control = list(maxit = 10, reltol = 0))
  result <- get_results(package = "pso", optimization_method = "psoptim",
                        parameters_for_optimization_method = opti_params_psoptim,
                        time_units = "secs", objective_name = "Duarte",
                        minimalistic_results = TRUE)
  result <- cbind(result, time_cluster)
  df <- rbind(df, result)

  # abc_optim
  opti_params_abc_optim <- list(par = starting_point, fn = Duarte,
                                lb = lower_bound, ub = upper_bound,
                                criter = .Machine$integer.max, maxCycle = 10)
  result <- get_results(package = "ABCoptim", optimization_method = "abc_optim",
                        parameters_for_optimization_method = opti_params_abc_optim,
                        time_units = "secs", objective_name = "Duarte",
                        minimalistic_results = TRUE)
  result <- cbind(result, time_cluster)
  df <- rbind(df, result)

  # sbplx
  opti_params_sbplx <- list(x0 = starting_point, fn = Duarte,
                            lower = lower_bound, upper = upper_bound,
                            control = list(maxeval = 100, xtol_rel = 0))
  result <- get_results(package = "nloptr", optimization_method = "sbplx",
                        parameters_for_optimization_method = opti_params_sbplx,
                        time_units = "secs", objective_name = "Duarte",
                        minimalistic_results = TRUE)
  result <- cbind(result, time_cluster)
  df <- rbind(df, result)

  # cobyla
  opti_params_cobyla <- list(x0 = starting_point, fn = Duarte,
                             lower = lower_bound, upper = upper_bound,
                             control = list(maxeval = 10, xtol_rel = 0))
  result <- get_results(package = "nloptr", optimization_method = "cobyla",
                        parameters_for_optimization_method = opti_params_cobyla,
                        time_units = "secs", objective_name = "Duarte",
                        minimalistic_results = TRUE)
  result <- cbind(result, time_cluster)
  df <- rbind(df, result)

  # bobyqa
  opti_params_bobyqa <- list(x0 = starting_point, fn = Duarte,
                             lower = lower_bound, upper = upper_bound,
                             control = list(maxeval = 17, xtol_rel = 0))
  result <- get_results(package = "nloptr", optimization_method = "bobyqa",
                        parameters_for_optimization_method = opti_params_bobyqa,
                        time_units = "secs", objective_name = "Duarte",
                        minimalistic_results = TRUE)
  result <- cbind(result, time_cluster)
  df <- rbind(df, result)

  # save all benchmark data for this time_cluster
  save_df_to_csv(df, output_file = file_path)
}
