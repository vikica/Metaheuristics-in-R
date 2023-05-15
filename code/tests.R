# R script for testing the optibenchmarks code
# Author: Jana Viktoria Kovacikova
# Date: May 14, 2023


source("/home/viki/Dokumenty/vikine_skolske/bakalarka_final/optibenchmarks/R/functions_for_benchmarking.R")
source("/home/viki/Dokumenty/vikine_skolske/bakalarka_final/optibenchmarks/R/optimization_problems.R")


# 2D Rosenbrock function
Rosenbrock <- function(x) {
  (1 - x[1])^2 + 100 * (x[2] - x[1]^2)^2
}

minus_Rosenbrock <- function(x) {
  return (- Rosenbrock(x))
}

lower_bound <- c(-5, -5)
upper_bound <- c(5, 5)
starting_point <- c(2, -1)


get_results(package = "stats", optimization_method = "optim",
            parameters_for_optimization_method = list(
              par = c(2, -1), fn = Rosenbrock, method = "L-BFGS-B",
              lower = c(-5, -5), upper = c(5, 5),
              control=list(maxit=1000)))

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

result <- psoptim(par=starting_point, fn=Rosenbrock, lower = lower_bound, 
                  upper = upper_bound, control = list(maxit = 200, reltol = 0))
get_output_data("pso", result)


dim <- 10
Rosenbrock <- makeRosenbrockFunction(dimensions = dim)
starting_point <- runif(n = dim, min = -5, max = 5)
lower_bound <- rep(-5, dim)
upper_bound <- rep(5, dim)
run_and_save(package = "stats", optimization_method = "optim",
             parameters_for_optimization_method = list(
               starting_point, Rosenbrock, method = "L-BFGS-B",
               lower = lower_bound, upper = upper_bound,
               control=list(maxit=1000)),
             output_file = "/home/viki/Dokumenty/vikine_skolske/bakalarka_final/export.csv",
             objective_name = "Rosenbrock 10D")
run_and_save(package = "GA", optimization_method = "ga", 
             parameters_for_optimization_method = list(
               type=c("real-valued"), fitness = minus_Rosenbrock, 
               lower = lower_bound, upper = upper_bound, maxiter = 100),
             output_file = "/home/viki/Dokumenty/vikine_skolske/bakalarka_final/export.csv",
             objective_name = "Rosenbrock 10D")
run_and_save(package = "DEoptim", optimization_method = "DEoptim", 
             parameters_for_optimization_method = list(
               Rosenbrock, lower = lower_bound, upper = upper_bound,
               control = list(itermax=500)),
             output_file = "/home/viki/Dokumenty/vikine_skolske/bakalarka_final/export.csv",
             objective_name = "Rosenbrock 10D")
run_and_save(package = "pso", optimization_method = "psoptim", 
             parameters_for_optimization_method = list(
               starting_point, fn=Rosenbrock, lower = lower_bound, 
               upper = upper_bound, control = list(maxit = 200, reltol = 0)),
             output_file = "/home/viki/Dokumenty/vikine_skolske/bakalarka_final/export.csv",
             objective_name = "Rosenbrock 10D")
run_and_save(package = "nloptr", optimization_method = "cobyla", 
             parameters_for_optimization_method = list(
               x0=starting_point, fn=Rosenbrock, lower = lower_bound, 
               upper = upper_bound, control = list(maxeval = 500, xtol_rel = 0)),
             output_file = "/home/viki/Dokumenty/vikine_skolske/bakalarka_final/export.csv",
             objective_name = "Rosenbrock 10D")
run_and_save(package = "nloptr", optimization_method = "bobyqa", 
             parameters_for_optimization_method = list(
               starting_point, fn=Rosenbrock, lower = lower_bound, 
               upper = upper_bound, control = list(maxeval = 500, xtol_rel = 0)),
             output_file = "/home/viki/Dokumenty/vikine_skolske/bakalarka_final/export.csv",
             objective_name = "Rosenbrock 10D")
run_and_save(package = "nloptr", optimization_method = "sbplx", 
             parameters_for_optimization_method = list(
               starting_point, fn=Rosenbrock, lower = lower_bound, 
               upper = upper_bound, control = list(maxeval = 500, xtol_rel = 0)),
             output_file = "/home/viki/Dokumenty/vikine_skolske/bakalarka_final/export.csv",
             objective_name = "Rosenbrock 10D")
run_and_save(package = "ABCoptim", optimization_method = "abc_optim", 
             parameters_for_optimization_method = list(
               par=starting_point, fn=Rosenbrock, lb=lower_bound, ub=upper_bound, 
               criter=.Machine$integer.max, maxCycle=100),
             output_file = "/home/viki/Dokumenty/vikine_skolske/bakalarka_final/export.csv",
             objective_name = "Rosenbrock 10D")

N <- 6
starting_point <- 2*runif(2*N) - 1
lower_bound <- rep(-1, 2*N)
upper_bound <- rep(1, 2*N)
run_and_save(package = "stats", optimization_method = "optim",
             parameters_for_optimization_method = list(
               starting_point, Lall, method = "L-BFGS-B",
               lower = lower_bound, upper = upper_bound,
               control=list(maxit=1000)),
             output_file = "/home/viki/Dokumenty/vikine_skolske/bakalarka_final/export.csv",
             objective_name = "Lall")
run_and_save(package = "GA", optimization_method = "ga", 
             parameters_for_optimization_method = list(
               type=c("real-valued"), fitness = minus_Lall, 
               lower = lower_bound, upper = upper_bound, maxiter = 100),
             output_file = "/home/viki/Dokumenty/vikine_skolske/bakalarka_final/export.csv",
             objective_name = "Lall")
run_and_save(package = "DEoptim", optimization_method = "DEoptim", 
             parameters_for_optimization_method = list(
               Lall, lower = lower_bound, upper = upper_bound,
               control = list(itermax=500)),
             output_file = "/home/viki/Dokumenty/vikine_skolske/bakalarka_final/export.csv",
             objective_name = "Lall")
run_and_save(package = "pso", optimization_method = "psoptim", 
             parameters_for_optimization_method = list(
               starting_point, fn=Lall, lower = lower_bound, 
               upper = upper_bound, control = list(maxit = 200, reltol = 0)),
             output_file = "/home/viki/Dokumenty/vikine_skolske/bakalarka_final/export.csv",
             objective_name = "Lall")
run_and_save(package = "nloptr", optimization_method = "cobyla", 
             parameters_for_optimization_method = list(
               x0=starting_point, fn=Lall, lower = lower_bound, 
               upper = upper_bound, control = list(maxeval = 500, xtol_rel = 0)),
             output_file = "/home/viki/Dokumenty/vikine_skolske/bakalarka_final/export.csv",
             objective_name = "Lall")
run_and_save(package = "nloptr", optimization_method = "bobyqa", 
             parameters_for_optimization_method = list(
               starting_point, fn=Lall, lower = lower_bound, 
               upper = upper_bound, control = list(maxeval = 500, xtol_rel = 0)),
             output_file = "/home/viki/Dokumenty/vikine_skolske/bakalarka_final/export.csv",
             objective_name = "Lall")
run_and_save(package = "nloptr", optimization_method = "sbplx", 
             parameters_for_optimization_method = list(
               starting_point, fn=Lall, lower = lower_bound, 
               upper = upper_bound, control = list(maxeval = 500, xtol_rel = 0)),
             output_file = "/home/viki/Dokumenty/vikine_skolske/bakalarka_final/export.csv",
             objective_name = "Lall")
run_and_save(package = "ABCoptim", optimization_method = "abc_optim", 
             parameters_for_optimization_method = list(
               par=starting_point, fn=Lall, lb=lower_bound, ub=upper_bound, 
               criter=.Machine$integer.max, maxCycle=100),
             output_file = "/home/viki/Dokumenty/vikine_skolske/bakalarka_final/export.csv",
             objective_name = "Lall")


N <- 100
lower_bound <- rep(-1, 3*N)
upper_bound <- rep(1, 3*N)
starting_point <- 2*runif(3*N) - 1
run_and_save(package = "stats", optimization_method = "optim",
             parameters_for_optimization_method = list(
               starting_point, Duarte, method = "L-BFGS-B",
               lower = lower_bound, upper = upper_bound,
               control=list(maxit=1000)),
             output_file = "/home/viki/Dokumenty/vikine_skolske/bakalarka_final/export.csv",
             objective_name = "Duarte")
run_and_save(package = "GA", optimization_method = "ga", 
             parameters_for_optimization_method = list(
               type=c("real-valued"), fitness = minus_Duarte, 
               lower = lower_bound, upper = upper_bound, maxiter = 50),
             output_file = "/home/viki/Dokumenty/vikine_skolske/bakalarka_final/export.csv",
             objective_name = "Duarte")
run_and_save(package = "DEoptim", optimization_method = "DEoptim", 
             parameters_for_optimization_method = list(
               Duarte, lower = lower_bound, upper = upper_bound,
               control = list(itermax=20)),
             output_file = "/home/viki/Dokumenty/vikine_skolske/bakalarka_final/export.csv",
             objective_name = "Duarte")
run_and_save(package = "pso", optimization_method = "psoptim", 
             parameters_for_optimization_method = list(
               starting_point, fn=Duarte, lower = lower_bound, 
               upper = upper_bound, control = list(maxit = 200, reltol = 0)),
             output_file = "/home/viki/Dokumenty/vikine_skolske/bakalarka_final/export.csv",
             objective_name = "Duarte")
run_and_save(package = "nloptr", optimization_method = "cobyla", 
             parameters_for_optimization_method = list(
               x0=starting_point, fn=Duarte, lower = lower_bound, 
               upper = upper_bound, control = list(maxeval = 50, xtol_rel = 0)),
             output_file = "/home/viki/Dokumenty/vikine_skolske/bakalarka_final/export.csv",
             objective_name = "Duarte")
run_and_save(package = "nloptr", optimization_method = "bobyqa", 
             parameters_for_optimization_method = list(
               starting_point, fn=Duarte, lower = lower_bound, 
               upper = upper_bound, control = list(maxeval = 50, xtol_rel = 0)),
             output_file = "/home/viki/Dokumenty/vikine_skolske/bakalarka_final/export.csv",
             objective_name = "Duarte")
run_and_save(package = "nloptr", optimization_method = "sbplx", 
             parameters_for_optimization_method = list(
               starting_point, fn=Duarte, lower = lower_bound, 
               upper = upper_bound, control = list(maxeval = 50, xtol_rel = 0)),
             output_file = "/home/viki/Dokumenty/vikine_skolske/bakalarka_final/export.csv",
             objective_name = "Duarte")
run_and_save(package = "ABCoptim", optimization_method = "abc_optim", 
             parameters_for_optimization_method = list(
               par=starting_point, fn=Duarte, lb=lower_bound, ub=upper_bound, 
               criter=.Machine$integer.max, maxCycle=40),
             output_file = "/home/viki/Dokumenty/vikine_skolske/bakalarka_final/export.csv",
             objective_name = "Duarte")

