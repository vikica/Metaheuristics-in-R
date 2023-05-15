# The definitions of problems that we optimize in our bachelor thesis


# The installation of package "smoof" may be necessary to create 
# multidimensional Rosenbrock function
# install.packages("smoof")
library("smoof")  


#' Problem 3.4. from article https://doi.org/10.1080/00949655.2018.1425689,
#' called "PROBLEM A" in our bachelor thesis
#'
#' Problem 3.4. from article https://doi.org/10.1080/00949655.2018.1425689,
#' called "PROBLEM A" in our bachelor thesis. The problem is to find a D-optimal 
#' design for a logistic regression model. The function is ready to be passed 
#' into optimization methods. The result is modified in such a way that the 
#' problem is more numerically stable, while the D-optimality is preserved.
#'
#' @param x The vectorized experiment.
#' @return Minus logarithm of the calculated determinant of Fisher information
#' matrix. If the determinant is smaller than 1e-100, the log of 1e-100 is 
#' returned in order to avoid calculating the logarithm of 0.
#' @export
Lall <- function(x) {
  th <- c(-1, 2, 0.5, 2, 0.1, 0.01)
  
  X <- matrix(x, ncol = 2, byrow = TRUE)
  Fl <- cbind(1, X[, 1], X[, 1]^2, X[, 2], X[, 2]^2, X[, 1]*X[, 2])
  eta <- Fl %*% th
  Fx <- (sqrt(exp(eta)/(1 + exp(eta))^2) %*% rep(1, 6)) * Fl
  # we choose max from {det(crossprod(Fx)), 1e-100} in order to avoid numerical 
  # instability
  dt <- max(det(crossprod(Fx)), 1e-100)
  # we apply natural logarithm in order to avoid numerical instability
  return(-log(dt))
}


#' Model 18 from article Duarte at al. https://doi.org/10.1007/s11222-017-9741-y,
#' called "PROBLEM B" in our bachelor thesis
#'
#' Model 18 from article Duarte at al. https://doi.org/10.1007/s11222-017-9741-y,
#' also used as a benchmark Model 4 in https://doi.org/10.1007/s11222-021-10046-2,
#' and called "PROBLEM B" in our bachelor thesis. The problem is to find a 
#' D-optimal design for a Poisson regression model. The function is ready to be 
#' passed into optimization methods. The result is modified in such a way that 
#' the problem is more numerically stable, while the D-optimality is preserved.
#'
#' @param x The vectorized experiment.
#' @return Minus logarithm of the calculated determinant of Fisher information
#' matrix. If the determinant is smaller than 1e-100, the log of 1e-100 is 
#' returned in order to avoid calculating the logarithm of 0.
#' @export
Duarte <- function(x) {
  th <- c(0.5, -0.2, 0.5, -0.2, -0.1, 0.2, -0.1, 0.2, -0.1, 0.2)
  
  X <- matrix(x, ncol = 3, byrow = TRUE)
  Fl <- cbind(1, X, X^2, X[,1]*X[,2], X[,1]*X[,3], X[,2]*X[,3])
  Fx <- (sqrt(exp(Fl %*% th)) %*% rep(1, 10)) * Fl
  dt <- max(det(crossprod(Fx)), 1e-100)
  return(-log(dt))
}


# multidimensional Rosenbrock function example
# (created directly in the benchmarking script with various dimensions)
dim <- 20
Rosenbrock <- makeRosenbrockFunction(dimensions = dim) 


# minus functions to be passed in GA::ga (because it solves maximization problems)
minus_Rosenbrock <- function(x) {
  return (-Rosenbrock(x))
}

minus_Lall <- function(x) {
  return (-(Lall(x)))
}

minus_Duarte <- function(x) {
  return (-(Duarte(x)))
}

