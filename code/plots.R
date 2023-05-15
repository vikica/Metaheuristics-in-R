# R script for plotting the results of opti benchmarks
# Author: Jana Viktoria Kovacikova
# Date: May 15, 2023

setwd("/home/viki/Dokumenty/vikine_skolske/bakalarka_final/Metaheuristics-in-R")
source("code/best_solutions.R")
# installation may be necessary
# install.packages("dplyr")
library("ggplot2")
library("dplyr")


#' Prepare data for plots.
#'
#' Create derived columns with suitable data for plots:
#' Remove data with missing information about optimal value,
#' apply logarithm on the column 'execution_time' (x-axis),
#' transform the minimum value according to rule provided in
#' parameter 'what_to_do_with_y', add medians of x and y values within each
#' time_cluster.
#'
#' @param df: A dataframe containing data about optimization results. It must
#' contain columns "value", "method", "R_package", "execution_time",
#' "time_cluster".
#' @param what_to_do_with_value: A string, one of {"transform log",
#' "nothing", "make differences"}. It specifies
#' which operation is supposed to be applied on the column 'value'. This
#' transformed column will then be our y in the plot.
#' "transform log": The decadic logarithm is applied.
#' "nothing": Nothing is applied, the real 'value' is taken.
#' "make differences": The difference between the current 'value' and
#' the best known value for the problem is calculated. Then, logarithm is applied.
#' @param small_const_to_ad: A float or int, default = 0. Specifies how
#' the minimum_value is supposed to be shifted before applying the logarithm
#' (in case what_to_do_with_value == "transform log" or "make differences" is
#' selected).
#' @param best_known_value: A float, the best known solution to the current
#' optimization problem. This is required if
#' what_to_do_with_value == "make differences" is selected.
#'
#' @return df: A dataframe containing new derived columns, ready to be
#' plotted.
prepare_data <- function(df, what_to_do_with_value, small_const_to_add = 0,
                         best_known_value=NULL) {

  # columns execution_timeas well as time_cluster are probably of
  # class 'character', which is wrong -> let's convert it to numeric:
  df$execution_time_seconds <- as.numeric(df$execution_time)
  df$time_cluster <- as.numeric(df$time_cluster)

  if (what_to_do_with_value == "nothing") {
    df['y'] <- df$value
  }
  else if (what_to_do_with_value == "transform log") {
    # add a small constant to a minimum value
    # (in order to be able to create a nice plot with log axis):
    df['value_plus_small_const'] <-
      df$value + small_const_to_add
    # apply the logarithm (this will be our y):
    df['y'] <- log10(df$value_plus_small_const)
  }
  else {
    difference <- create_difference_from_the_best_known_value(df, best_known_value)
    df['y'] <- log10(difference + small_const_to_add)
  }

  # apply the decadic logarithm on execution time (= x axis):
  df['log_execution_time'] <- log10(df$execution_time)
  df['log_time_cluster'] <- log10(df$time_cluster)

  # add medians of execution times and values within each "cluster":
  # ...a "cluster" is a set of values sharing the same method and the same
  # time_cluster.
  df <- add_medians(df)

  return(df)
}


#' Create a column with difference from the best known minimum value
#' ("global optimum").
#'
#' @param df: A dataframe containing a column "minimum_value"
#' @param best_known_value: The best known minimum value for the given
#' optimization problem.
#'
#' @return: A new column "difference_from_best_known_val"
create_difference_from_the_best_known_value <- function(df, best_known_value) {
  df["difference_from_best_known_val"] <- df["minimum_value"] - best_known_value
  return(df["difference_from_best_known_val"])
}


#' Add medians of execution times and minimum values within each "cluster".
#'
#' This function adds medians of execution times and minimum values within
#' each "cluster" of a dataset. A cluster is defined as set of data points
#' that share the same method and the same time_cluster value.
#'
#' @param data A dataframe containing columns 'method', 'time_cluster',
#' 'log_execution_time', 'y'.
#
#' @return data The original dataframe with 2 new columns:
#' 'median_execution_time': The median of log_execution_time within the cluster,
#' 'median_y': The median of y within the cluster.
add_medians <- function(data) {

  data <- data %>%
    group_by(method, time_cluster) %>%
    mutate(median_execution_time = median(log_execution_time))

  data <- data %>%
    group_by(method, time_cluster) %>%
    mutate(median_y = median(y))

  return(data)
}


#' Remove rows with method==method_name from the dataframe
#'
#' @param df A dataframe containing a column 'method'.
#'
#' @return The dataframe where rows with method == method_name are no longer
#' present.
remove_method <- function(df, method_name) {
  df <- df[df$method != method_name, ]
  return(df)
}


draw_plot <- function(data, title, breaks, y_label, limits = NULL) {
  # rename the methods
  new_labels <- c("optim" = "optim L-BFGS-B", "DEoptim" = "DEoptim",
                  "ga" = "ga", "psoptim" = "psoptim", "cobyla" = "cobyla",
                  "bobyqa" = "bobyqa", "sbplx" = "sbplx",
                  "abc_optim" = "abc_optim")

  ggplot() +
    geom_point(data = data.frame(x = data$log_execution_time,
                                 y = data$y,
                                 method = data$method),
               aes(x, y, colour = method, group = method)) +
    geom_line(data = data.frame(x = data$median_execution_time,
                                y = data$median_y,
                                method = data$method),
              aes(x, y, colour = method, group = method)) +
    geom_point(data = data.frame(x = data$median_execution_time,
                                 y = data$median_y,
                                 method = data$method),
               aes(x, y, fill = method, group = method),
               size = 3.5, shape = 24, colour = "white") +
    # theme_bw() +
    labs(title = title) +
    scale_x_continuous(breaks = breaks, limits = limits) +
    xlab("log10 of (execution time [s])") +
    ylab(y_label) +
    scale_color_discrete() +
    scale_fill_discrete()
}




# load the data
Rosenbrock3D <- read.csv("results/Rosenbrock3D.csv")
Rosenbrock3D_ready <- prepare_data(Rosenbrock3D,
                                   what_to_do_with_value = "transform log",
                                   small_const_to_add = 1e-6)
draw_plot(Rosenbrock3D_ready, "Rosenbrock 3D", breaks=c(-3, -2),
          y_label = "log10 of (minimum value+1e-6)")


