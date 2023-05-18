# R script for plotting the results of opti benchmarks
# Author: Jana Viktoria Kovacikova
# Date: May 15, 2023

setwd("/home/viki/Dokumenty/vikine_skolske/bakalarka_final/Metaheuristics-in-R")
source("code/best_solutions.R")
# installation may be necessary
# install.packages("dplyr", "ggplot2")
library("ggplot2")
library("dplyr")


#' Prepare data for plots.
#'
#' Create derived columns with suitable data for plots:
#' Remove data with missing information about optimal value,
#' apply logarithm on the column 'execution_time' (x-axis),
#' transform the minimum value according to rule provided in
#' parameter 'what_to_do_with_y', add medians of x and y values within each
#' time_cluster, and sort the dataframe by method.
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

  # sort the dataframe by method
  df <- sort_by_method(df)

  return(df)
}


#' Sort dataframe by colum 'method'
#'
#' Sort dataframe by colum 'method', using a custom order
#'
#' @param df: A dataframe containing data about optimization results. It must
#' contain column "method" with possible values "ga", "DEoptim", "abc_optim",
#' "psoptim", "optim", "sbplx", "cobyla", "bobyqa".
#' @return df: A sorted dataframe by method.
sort_by_method <- function(df) {
  # Define the custom order
  custom_order <- c("ga", "DEoptim", "abc_optim", "psoptim", "optim", "sbplx", "cobyla", "bobyqa")
  # Convert the "method" column to a factor with the custom order
  df$method <- factor(df$method, levels = custom_order)
  # Sort the dataframe
  df <- df[order(df$method), ]
  return(df)
}


#' Create a column with difference from the best known minimum value
#' ("global optimum").
#'
#' @param df: A dataframe containing a column "value"
#' @param best_known_value: The best known minimum value for the given
#' optimization problem.
#'
#' @return: A new column "difference_from_best_known_val"
create_difference_from_the_best_known_value <- function(df, best_known_value) {
  df["difference_from_best_known_val"] <- df["value"] - best_known_value
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


#' Draw plot of optimization benchmarking data
#'
#' @param data A dataframe prepared for plotting (must contain the columns:
#' method, log_execution_time, y, median_execution_time, median_y), for example
#' result of data preprocessing by the function 'prepare_data'.
#' @param title The title of the plot.
#' @param breaks A vector of the breaks on x-axis.
#' @param y_label The text label of the y-axis.
#' @param best_known_val The corresponding value of the global optimum in the
#' plot, or expected global optimum. In case the values in column y were
#' transformed, the same transformation on best_known_val is expected to be
#' entered. For example, if y is a logarithm of the value, best_known_val
#' should also be a logarithm (of the global optimum). This value is used for
#' plotting a horizontal grey line.
#' @param limits A vector containing limits on x-axis displayed. Default is NULL,
#' in which case the x-axis is wide enough to display all the values.
#' @param label_line A label of the horizontal line displaying the expected
#' global optimum.
draw_plot <- function(data, title, breaks, y_label, best_known_val,
                      limits = NULL, label_line = "") {
  # rename the methods
  new_labels <- c("optim" = "L-BFGS-B", "DEoptim" = "DE",
                  "ga" = "GA", "psoptim" = "PSO", "cobyla" = "COBYLA",
                  "bobyqa" = "BOBYQA", "sbplx" = "SBPLX",
                  "abc_optim" = "ABC")
  # define the color palette
  palette <- c("#fccfd0", "#7EC8E3", "#4bccc3", "#A28EA2", "#485df7", "#EC1C20",
               "#91052B", "#000C66")
  if (is.null(limits)) {
    min_x <- min(data$log_execution_time)
    max_x <- max(data$log_execution_time)
    limits <- c(min_x, max_x)
  }

  ggplot() +
    geom_hline(yintercept = best_known_val, linetype = "dashed", color = "grey") +
    geom_text(aes(x = min_x, y = -6, label = label_line), hjust = 0,
              vjust = -0.5, color = "grey", size = 3.2) +
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
    labs(title = title) +
    scale_x_continuous(breaks = breaks, limits = limits) +
    xlab("log10 of (execution time [s])") +
    ylab(y_label) +
    scale_color_manual(values = c(palette), labels = new_labels) +
    scale_fill_manual(values = c(palette), labels = new_labels)
}


# load the optimization benchmarking data and plot them

Rosenbrock3D <- read.csv("results/Rosenbrock3D.csv")
Rosenbrock3D_ready <- prepare_data(Rosenbrock3D,
                                   what_to_do_with_value = "transform log",
                                   small_const_to_add = 1e-6)
draw_plot(Rosenbrock3D_ready, "Rosenbrock 3D", breaks=c(-3, -2, -1, 0, 1),
          y_label = "log10 of (minimum value+1e-6)",
          best_known_val = -6, label_line = "global minimum")
ggsave(filename = "results/plots/Rosenbrock3D.png", width = 7, height = 4.5,
       dpi=700)

Rosenbrock10D <- read.csv("results/Rosenbrock10D.csv")
Rosenbrock10D_ready <- prepare_data(Rosenbrock10D,
                                    what_to_do_with_value = "transform log",
                                    small_const_to_add = 1e-6)
draw_plot(Rosenbrock10D_ready, "Rosenbrock 10D", breaks=c(-2, -1, 0, 1, 2),
          y_label = "log10 of (minimum value+1e-6)",
          best_known_val = -6, label_line = "global minimum")
ggsave(filename = "results/plots/Rosenbrock10D.png", width = 7, height = 4.5,
       dpi=700)

Rosenbrock20D <- read.csv("results/Rosenbrock20D.csv")
Rosenbrock20D_ready <- prepare_data(Rosenbrock20D,
                                    what_to_do_with_value = "transform log",
                                    small_const_to_add = 1e-6)
draw_plot(Rosenbrock20D_ready, "Rosenbrock 20D", breaks=c(-2, -1, 0, 1, 2),
          y_label = "log10 of (minimum value+1e-6)",
          best_known_val = -6, label_line = "global minimum")
ggsave(filename = "results/plots/Rosenbrock20D.png", width = 7, height = 4.5,
       dpi=700)

Rosenbrock50D <- read.csv("results/Rosenbrock50D.csv")
Rosenbrock50D_ready <- prepare_data(Rosenbrock50D,
                                    what_to_do_with_value = "transform log",
                                    small_const_to_add = 1e-6)
draw_plot(Rosenbrock50D_ready, "Rosenbrock 50D", breaks=c(-2, -1, 0, 1, 2),
          y_label = "log10 of (minimum value+1e-6)",
          best_known_val = -6, label_line = "global minimum")
ggsave(filename = "results/plots/Rosenbrock50D.png", width = 7, height = 4.5,
       dpi=700)
