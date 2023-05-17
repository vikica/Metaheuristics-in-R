# R script for benchmarking optimization methods in R
# Author: Jana Viktoria Kovacikova
# Date: May 15, 2023


source("/home/viki/Dokumenty/vikine_skolske/bakalarka_final/optibenchmarks/R/functions_for_benchmarking.R")
source("/home/viki/Dokumenty/vikine_skolske/bakalarka_final/optibenchmarks/R/optimization_problems.R")
library("smoof")


# this is an example of how we benchmarked the optimization methods,
# it is rather a template, not a complete script

# Rosenbrock
dimension <- 10
Rosenbrock <- makeRosenbrockFunction(dimensions = dimension)
file_path <- sprintf("/home/viki/Dokumenty/vikine_skolske/bakalarka_final/Metaheuristics-in-R/results/Rosenbrock%dD_4.csv",
                     dimension)
# set the lower and upper bound
lower_bound <- rep(-5, dimension)
upper_bound <- rep(5, dimension)
# generate 20 starting points for each time_cluster
starting_points <- list()
for (i in 1:20) {
  starting_points <- c(starting_points,
                       list(runif(n = dimension, min = lower_bound,
                                  max = upper_bound)))
}

# benchmark all opti methods using the same 20 starting_points
for (starting_point in starting_points) {
  # the desired execution time time_cluster is to be set,
  # as well as number of iterations/evaluations in each method
  time_cluster <- 100
  df <- data.frame()

  # optim L-BFGS-B
  # opti_params_lbfgs <- list(par = starting_point, fn = Rosenbrock,
  #                           method = "L-BFGS-B", lower = lower_bound,
  #                           upper = upper_bound,
  #                           control = list(maxit = 10000, factr = 0))
  # result <- get_results(package = "stats", optimization_method = "optim",
  #                       parameters_for_optimization_method = opti_params_lbfgs,
  #                       time_units = "secs", objective_name = "Rosenbrock")
  # result <- cbind(result, time_cluster)
  # df <- result

  # ga
  opti_params_ga <- list(type=c("real-valued"), fitness = minus_Rosenbrock,
                         lower = lower_bound, upper = upper_bound,
                         maxiter = 39000)
  result <- get_results(package = "GA", optimization_method = "ga",
                        parameters_for_optimization_method = opti_params_ga,
                        time_units = "secs", objective_name = "Rosenbrock")
  result <- cbind(result, time_cluster)
  df <- rbind(df, result)

  # DEoptim
  opti_params_DEoptim <- list(fn = Rosenbrock, lower = lower_bound,
                              upper = upper_bound,
                              control = list(itermax = 53000, reltol = 0))
  result <- get_results(package = "DEoptim", optimization_method = "DEoptim",
                        parameters_for_optimization_method = opti_params_DEoptim,
                        time_units = "secs", objective_name = "Rosenbrock")
  result <- cbind(result, time_cluster)
  df <- rbind(df, result)

  # psoptim
  opti_params_psoptim <- list(par=starting_point, fn=Rosenbrock,
                              lower = lower_bound, upper = upper_bound,
                              control = list(maxit = 160000, reltol = 0))
  result <- get_results(package = "pso", optimization_method = "psoptim",
                        parameters_for_optimization_method = opti_params_psoptim,
                        time_units = "secs", objective_name = "Rosenbrock")
  result <- cbind(result, time_cluster)
  df <- rbind(df, result)

  # abc_optim
  opti_params_abc_optim <- list(par = starting_point, fn= Rosenbrock,
                                lb = lower_bound, ub = upper_bound,
                                criter = .Machine$integer.max, maxCycle = 80000)
  result <- get_results(package = "ABCoptim", optimization_method = "abc_optim",
                        parameters_for_optimization_method = opti_params_abc_optim,
                        time_units = "secs", objective_name = "Rosenbrock")
  result <- cbind(result, time_cluster)
  df <- rbind(df, result)

  # sbplx
  opti_params_sbplx <- list(x0 = starting_point, fn = Rosenbrock,
                            lower = lower_bound, upper = upper_bound,
                            control = list(maxeval = 3200000, xtol_rel = 0))
  result <- get_results(package = "nloptr", optimization_method = "sbplx",
                        parameters_for_optimization_method = opti_params_sbplx,
                        time_units = "secs", objective_name = "Rosenbrock")
  result <- cbind(result, time_cluster)
  df <- rbind(df, result)

  # cobyla
  opti_params_cobyla <- list(x0 = starting_point, fn = Rosenbrock,
                             lower = lower_bound, upper = upper_bound,
                             control = list(maxeval = 6000000, xtol_rel = 1e-15))
  result <- get_results(package = "nloptr", optimization_method = "cobyla",
                        parameters_for_optimization_method = opti_params_cobyla,
                        time_units = "secs", objective_name = "Rosenbrock")
  result <- cbind(result, time_cluster)
  df <- rbind(df, result)

  # bobyqa
  # opti_params_bobyqa <- list(x0 = starting_point, fn = Rosenbrock,
  #                            lower = lower_bound, upper = upper_bound,
  #                            control = list(maxeval = 28000,
  #                                           xtol_rel = 0))
  # result <- get_results(package = "nloptr", optimization_method = "bobyqa",
  #                       parameters_for_optimization_method = opti_params_bobyqa,
  #                       time_units = "secs", objective_name = "Rosenbrock")
  # result <- cbind(result, time_cluster)
  # df <- rbind(df, result)


  # save all benchmark data for this time_cluster
  save_df_to_csv(df, output_file = file_path)
}
