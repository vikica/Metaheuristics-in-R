# R script for benchmarking optimization methods in R
# Author: Jana Viktoria Kovacikova
# Date: May 15, 2023


source("/home/viki/Dokumenty/vikine_skolske/bakalarka_final/optibenchmarks/R/functions_for_benchmarking.R")
source("/home/viki/Dokumenty/vikine_skolske/bakalarka_final/optibenchmarks/R/optimization_problems.R")
library("smoof")


# this is an example of how we benchmarked the optimization methods


# benchmark Rosenbrock of dimensions 3, 10, 20, 50
# the desired times for each dimension of Rosenbrock
time_clusters <- list("3" = c(0.001, 0.01, 0.1, 1, 10),
                      "10" = c(0.01, 0.1, 1, 10, 100),
                      "20" = c(0.01, 0.1, 1, 10, 100),
                      "50" = c(0.01, 0.1, 1, 10, 100))
files <- list("3" = "/home/viki/Dokumenty/vikine_skolske/bakalarka_final/Metaheuristics-in-R/results/Rosenbrock3D.csv",
              "10" = c(0.01, 0.1, 1, 10, 100),
              "20" = c(0.01, 0.1, 1, 10, 100),
              "50" = c(0.01, 0.1, 1, 10, 100))
for (dimension in c(3, 10, 20, 50)) {
  Rosenbrock <- makeRosenbrockFunction(dimensions = dimension)
  file_path <- sprintf("/home/viki/Dokumenty/vikine_skolske/bakalarka_final/Metaheuristics-in-R/results/Rosenbrock%dD.csv",
                       dimension)
  # set the lower and upper bound
  lower_bound <- rep(-5, dimension)
  upper_bound <- rep(5, dimension)

  for (time_cluster in time_clusters[paste(dimension)]) {
    # generate 20 starting points for each time_cluster
    starting_points <- list()
    for (i in 1:20) {
      starting_points <- c(starting_points,
                           list(runif(n = dimension, min = lower_bound,
                                      max = upper_bound)))
    }
    # benchmark all opti methods using the same 20 starting_points
    benchmark_ga(time_cluster, starting_points, lower_bound, upper_bound,
                 objective = Rosenbrock, output_file = file_path,
                 dimension = dimension, objective_name = "Rosenbrock")
    benchmark_DEoptim(time_cluster, starting_points, lower_bound, upper_bound,
                      objective = Rosenbrock, output_file = file_path,
                      dimension = dimension, objective_name = "Rosenbrock")
    benchmark_psoptim(time_cluster, starting_points, lower_bound, upper_bound,
                      objective = Rosenbrock, output_file = file_path,
                      dimension = dimension, objective_name = "Rosenbrock")
    benchmark_ABCoptim(time_cluster, starting_points, lower_bound, upper_bound,
                       objective = Rosenbrock, output_file = file_path,
                       dimension = dimension, objective_name = "Rosenbrock")
    benchmark_LBFGSB(time_cluster, starting_points, lower_bound, upper_bound,
                     objective = Rosenbrock, output_file = file_path,
                     dimension = dimension, objective_name = "Rosenbrock")
    benchmark_sbplx(time_cluster, starting_points, lower_bound, upper_bound,
                    objective = Rosenbrock, output_file = file_path,
                    dimension = dimension, objective_name = "Rosenbrock")
    benchmark_cobyla(time_cluster, starting_points, lower_bound, upper_bound,
                     objective = Rosenbrock, output_file = file_path,
                     dimension = dimension, objective_name = "Rosenbrock")
    benchmark_bobyqa(time_cluster, starting_points, lower_bound, upper_bound,
                     objective = Rosenbrock, output_file = file_path,
                     dimension = dimension, objective_name = "Rosenbrock")
  }


}


benchmark_ga <- function(time_cluster, starting_points, lower_bound,
                         upper_bound, objective, output_file, dimension,
                         objective_name) {

}

benchmark_DEoptim <- function(time_cluster, starting_points, lower_bound,
                              upper_bound, objective, output_file, dimension,
                              objective_name) {

}

benchmark_psoptim <- function(time_cluster, starting_points, lower_bound,
                              upper_bound, objective, output_file, dimension,
                              objective_name) {

}

benchmark_ABCoptim <- function(time_cluster, starting_points, lower_bound,
                               upper_bound, objective, output_file, dimension,
                               objective_name) {

}

benchmark_LBFGSB <- function(time_cluster, starting_points, lower_bound,
                             upper_bound, objective, output_file, dimension,
                             objective_name) {
  numbers_of_iterations <- list("Rosenbrock" = list("3" = list("0.001" = 7, "0.01" = ),
                                                    "10" = list(),
                                                    "20" = list(),
                                                    "50" = list()),
                                "Lall" = list(),
                                "Duarte" = list())
  print(numbers_of_iterations[[objective_name]])
}
Rosenbrock <- makeRosenbrockFunction(dimensions = 3)
benchmark_LBFGSB(0.001, list(c(1, 1, 0), c(-5, -1, 2)), c(-5, -5, -5),
                 c(5, 5, 5), Rosenbrock,
                 "/home/viki/Dokumenty/vikine_skolske/bakalarka_final/Metaheuristics-in-R/results/test.csv",
                 3, "Rosenbrock")

benchmark_sbplx <- function(time_cluster, starting_points, lower_bound,
                            upper_bound, objective, output_file, dimension,
                            objective_name) {

}

benchmark_cobyla <- function(time_cluster, starting_points, lower_bound,
                             upper_bound, objective, output_file, dimension,
                             objective_name) {

}

benchmark_bobyqa <- function(time_cluster, starting_points, lower_bound,
                             upper_bound, objective, output_file, dimension,
                             objective_name) {

}


parameters_for_optimization_method <- list(
  starting_point, Rosenbrock, method = "L-BFGS-B", lower = lower_bound,
  upper = upper_bound, control=list(maxit=7))

result <- get_results(package, optimization_method, parameters_for_optimization_method, time_units = "secs", objective_name = "Rosenbrock")
result <- cbind(result, time_cluster)
save_df_to_csv(result, output_file = path)


run_and_save(package = "stats", optimization_method = "optim",
             parameters_for_optimization_method = list(
               c(2, -1), Rosenbrock, method = "L-BFGS-B",
               lower = c(-5, -5), upper = c(5, 5),
               control=list(maxit=1000)),
             output_file = "/home/viki/Dokumenty/vikine_skolske/bakalarka_final/export.csv",
             objective_name = "Rosenbrock 2D")

run_and_save(package = "GA", optimization_method = "ga",
             parameters_for_optimization_method = list(
               type=c("real-valued"), fitness = minus_Rosenbrock,
               lower = lower_bound, upper = upper_bound, maxiter = 100),
             output_file = "/home/viki/Dokumenty/vikine_skolske/bakalarka_final/export.csv",
             objective_name = "Rosenbrock 2D")

run_and_save(package = "DEoptim", optimization_method = "DEoptim",
             parameters_for_optimization_method = list(
               fn = Rosenbrock, lower = lower_bound, upper = upper_bound,
               control = list(itermax=500)),
             output_file = "/home/viki/Dokumenty/vikine_skolske/bakalarka_final/export.csv",
             objective_name = "Rosenbrock 2D")

get_results(package = "pso", optimization_method = "psoptim",
            parameters_for_optimization_method = list(
              par=starting_point, fn=Rosenbrock, lower = lower_bound,
              upper = upper_bound, control = list(maxit = 200, reltol = 0)))

run_and_save(package = "pso", optimization_method = "psoptim",
             parameters_for_optimization_method = list(
               par=starting_point, fn=Rosenbrock, lower = lower_bound,
               upper = upper_bound, control = list(maxit = 200, reltol = 0)),
             output_file = "/home/viki/Dokumenty/vikine_skolske/bakalarka_final/export.csv",
             objective_name = "Rosenbrock 2D")

run_and_save(package = "nloptr", optimization_method = "cobyla",
             parameters_for_optimization_method = list(
               x0=starting_point, fn=Rosenbrock, lower = lower_bound,
               upper = upper_bound, control = list(maxeval = 500, xtol_rel = 0)),
             output_file = "/home/viki/Dokumenty/vikine_skolske/bakalarka_final/export.csv",
             objective_name = "Rosenbrock 2D")

run_and_save(package = "nloptr", optimization_method = "bobyqa",
             parameters_for_optimization_method = list(
               x0=starting_point, fn=Rosenbrock, lower = lower_bound,
               upper = upper_bound, control = list(maxeval = 500, xtol_rel = 0)),
             output_file = "/home/viki/Dokumenty/vikine_skolske/bakalarka_final/export.csv",
             objective_name = "Rosenbrock 2D")

run_and_save(package = "nloptr", optimization_method = "sbplx",
             parameters_for_optimization_method = list(
               starting_point, fn=Rosenbrock, lower = lower_bound,
               upper = upper_bound, control = list(maxeval = 500, xtol_rel = 0)),
             output_file = "/home/viki/Dokumenty/vikine_skolske/bakalarka_final/export.csv",
             objective_name = "Rosenbrock 2D")

run_and_save(package = "ABCoptim", optimization_method = "abc_optim",
             parameters_for_optimization_method = list(
               par=starting_point, fn=Rosenbrock, lb=lower_bound, ub=upper_bound,
               criter=.Machine$integer.max, maxCycle=100),
             output_file = "/home/viki/Dokumenty/vikine_skolske/bakalarka_final/export.csv",
             objective_name = "Rosenbrock 2D")
