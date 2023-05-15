# R script for testing the optibenchmarks code
# Author: Jana Viktoria Kovacikova
# Date: May 14, 2023


source("/home/viki/Dokumenty/vikine_skolske/bakalarka_final/optibenchmarks/R/functions_for_benchmarking.R")


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
               x0=starting_point, fn=Rosenbrock, lower = lower_bound, 
               upper = upper_bound, control = list(maxeval = 500, xtol_rel = 0)),
             output_file = "/home/viki/Dokumenty/vikine_skolske/bakalarka_final/export.csv",
             objective_name = "Rosenbrock 2D")

run_and_save(package = "ABCoptim", optimization_method = "abc_optim", 
             parameters_for_optimization_method = list(
               par=starting_point, fn=Rosenbrock, lb=lower_bound, ub=upper_bound, 
               criter=.Machine$integer.max, maxCycle=100),
             output_file = "/home/viki/Dokumenty/vikine_skolske/bakalarka_final/export.csv",
             objective_name = "Rosenbrock 2D")

print(get_input_data("ABCoptim", 
                     list(par=starting_point, fn=Rosenbrock, lb=lower_bound, 
                          ub=upper_bound, criter=.Machine$integer.max, 
                          maxCycle=100)))


help("optim")
# optim: par, fn, kwargs = (method, lower, upper, control[maxit])
# $par $value $counts $convergence $message
help("ga")
# type, fitness, lower, upper, kwargs=maxiter
# class: iter, fitnessValue, solution
help("DEoptim")
# fn, lower, upper, control[itermax]
# class: bestmem, bestval, nfeval ( number of function evaluations), iter: number of procedure iterations.
help("pso")
# par, fn, kwargs = (lower, upper, control[maxit])
# par, value, convergence, message, counts (A three-element vector containing the number of function evaluations, the number of iterations, and the number of restarts.)
help("cobyla")
help("bobyqa")
help("sbplx")
# x0, fn, kwargs = (lower, upper), control[]
# par, value, iter, convergence, message
help("nl.opts")
help("abc_optim")
# par, fn, kwargs = (lb, ub, maxCycle, criter)
# class: value, par, counts

pck <- "pso"
pck %in% c("optim", "pso", "ABCoptim")

