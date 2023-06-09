# R script for plotting the results of opti benchmarks
# Author: Jana Viktoria Kovacikova
# Date: May 15, 2023


source("best_solutions.R")
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


  df['y'] <- df$value
  # add medians of execution times and values within each "cluster":
  # ...a "cluster" is a set of values sharing the same method and the same
  # time_cluster.
  df <- add_medians(df)

  if (what_to_do_with_value == "transform log") {
    # add a small constant to a minimum value
    # (in order to be able to create a nice plot with log axis):
    df['value_plus_small_const'] <-
      df$value + small_const_to_add
    # apply the logarithm (this will be our y):
    df['y'] <- log10(df$value_plus_small_const)
    df['median_y'] <- log10(df$median_y + small_const_to_add)
  }
  else if (what_to_do_with_value == "make differences") {
    difference <- create_difference_from_the_best_known_value(df, best_known_value)
    df['y'] <- difference
    df <- add_medians(df)
    df['y'] <- log10(difference + small_const_to_add)
    df['median_y'] <- log10(df$median_y + small_const_to_add)
  }

  # apply the decadic logarithm on execution time (= x axis):
  df['log_execution_time'] <- log10(df$execution_time)
  df['log_time_cluster'] <- log10(df$time_cluster)
  df['median_execution_time'] <- log10(df$median_execution_time)

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
#' 'execution_time', 'y'.
#
#' @return data The original dataframe with 2 new columns:
#' 'median_execution_time': The median of execution_time within the cluster,
#' 'median_y': The median of y within the cluster.
add_medians <- function(data) {

  data <- data %>%
    group_by(method, time_cluster) %>%
    mutate(median_execution_time = median(execution_time))

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
#' @param y_line_position The corresponding value of the global optimum in the
#' plot. This value is used for plotting a horizontal grey line.
#' @param limits A vector containing limits on x-axis displayed. Default is NULL,
#' in which case the x-axis is wide enough to display all the values.
#' @param label_line A label of the horizontal line displaying the expected
#' global optimum.
#' @param add_tick_minimum A boolean indicating whether a tick is to be added
#' next to the horizontal line of global minimum.
#' @param leave_out_y_ticks A vector of numbers which are be left out from ticks
#' of y-axis.
#' @param custom_y_breaks A vector of y-axis breaks.
draw_plot <- function(data, title, breaks, y_label, y_line_position = NULL,
                      limits = NULL, label_line = "", add_tick_minimum = FALSE,
                      leave_out_y_ticks = NULL, custom_y_breaks = NULL) {
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

  # Get default y-breaks values
  default_breaks <- pretty(data$y)
  if (!is.null(custom_y_breaks)) {
    default_breaks <- custom_y_breaks
  }
  if (add_tick_minimum & !is.null(y_line_position)) {
    if (!is.null(leave_out_y_ticks)) {
      # remove breaks that are specified by leave_out_y_ticks
      default_breaks <- default_breaks[!default_breaks %in% leave_out_y_ticks]
    }
    # Add the expected global minimum tick
    y_breaks <- c(default_breaks, y_line_position)
    truncated_best_val <- as.character(round(y_line_position, 2))
    y_tick_labels <- c(as.character(default_breaks),
                       paste("~", truncated_best_val))
  }
  else {
    y_breaks <- default_breaks
    y_tick_labels <- default_breaks
  }

  hline <- NULL
  line_label <- NULL
  if (!is.null(y_line_position)) {
    hline <- geom_hline(yintercept = y_line_position, linetype = "dashed",
                        color = "grey")
    line_label <- geom_text(aes(x = min_x, y = y_line_position,
                                label = label_line), hjust = 0, vjust = -0.5,
                            color = "grey", size = 3.2)
  }

  ggplot() +
    hline +
    line_label +
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
    scale_y_continuous(breaks = y_breaks, labels = y_tick_labels) +
    theme(
      panel.grid.major = element_line(color = "white"),
      panel.grid.minor = element_blank()
    ) +
    ylab(y_label) +
    scale_color_manual(values = c(palette), labels = new_labels) +
    scale_fill_manual(values = c(palette), labels = new_labels)
}




# load the optimization benchmarking data and plot the results

Rosenbrock3D <- read.csv("results/Rosenbrock3D.csv")
Rosenbrock3D_ready <- prepare_data(Rosenbrock3D,
                                   what_to_do_with_value = "transform log",
                                   small_const_to_add = 1e-6)
draw_plot(Rosenbrock3D_ready, "Rosenbrock 3D", breaks=c(-3, -2, -1, 0, 1),
          y_label = "log10 of (minimum value+1e-6)",
          y_line_position = -6, label_line = "global minimum")
ggsave(filename = "results/plots/Rosenbrock3D.png", width = 7, height = 4.5,
       dpi=700)

Rosenbrock10D <- read.csv("results/Rosenbrock10D.csv")
Rosenbrock10D_ready <- prepare_data(Rosenbrock10D,
                                    what_to_do_with_value = "transform log",
                                    small_const_to_add = 1e-6)
draw_plot(Rosenbrock10D_ready, "Rosenbrock 10D", breaks=c(-2, -1, 0, 1, 2),
          y_label = "log10 of (minimum value+1e-6)",
          y_line_position = -6, label_line = "global minimum")
ggsave(filename = "results/plots/Rosenbrock10D.png", width = 7, height = 4.5,
       dpi=700)

Rosenbrock20D <- read.csv("results/Rosenbrock20D.csv")
Rosenbrock20D_ready <- prepare_data(Rosenbrock20D,
                                    what_to_do_with_value = "transform log",
                                    small_const_to_add = 1e-6)
draw_plot(Rosenbrock20D_ready, "Rosenbrock 20D", breaks=c(-2, -1, 0, 1, 2),
          y_label = "log10 of (minimum value+1e-6)",
          y_line_position = -6, label_line = "global minimum")
ggsave(filename = "results/plots/Rosenbrock20D.png", width = 7, height = 4.5,
       dpi=700)

Rosenbrock50D <- read.csv("results/Rosenbrock50D.csv")
Rosenbrock50D_ready <- prepare_data(Rosenbrock50D,
                                    what_to_do_with_value = "transform log",
                                    small_const_to_add = 1e-6)
draw_plot(Rosenbrock50D_ready, "Rosenbrock 50D", breaks=c(-2, -1, 0, 1, 2),
          y_label = "log10 of (minimum value+1e-6)",
          y_line_position = -6, label_line = "global minimum")
ggsave(filename = "results/plots/Rosenbrock50D.png", width = 7, height = 4.5,
       dpi=700)

# calculate best found values
ga_values <- Rosenbrock10D$value[Rosenbrock10D$method == "ga"]
min(ga_values)
best_values_by_method <- Rosenbrock3D$value[Rosenbrock3D$method == "ga"]
min(best_values)

LallN6 <- read.csv("results/LallN6.csv")
LallN6_ready <- prepare_data(LallN6,
                             what_to_do_with_value = "nothing",
                             small_const_to_add = 0)
draw_plot(LallN6_ready, "Problem A, N=6", breaks=c(-3, -2, -1, 0, 1),
          y_label = "minimum value found",
          y_line_position = optimum_Lall_N6, label_line = "global minimum",
          add_tick_minimum = TRUE)
ggsave(filename = "results/plots/LallN6.png", width = 7, height = 4.5,
       dpi=700)
print(optimum_Lall_N6)

LallN6_ready <- prepare_data(LallN6, what_to_do_with_value = "make differences",
                             best_known_value = optimum_Lall_N6,
                             small_const_to_add = 1e-6)
draw_plot(LallN6_ready, "Problem A, N=6", breaks=c(-3, -2, -1, 0, 1),
          y_label = "log10 of (difference from the expected global minimum + 1e-6)",
          y_line_position = -6, label_line = "zero difference",
          custom_y_breaks = c(1, 0, -1, -2, -3, -4, -5, -6))
ggsave(filename = "results/plots/LallN6_2.png", width = 7, height = 4.5,
       dpi=700)

LallN24 <- read.csv("results/LallN24.csv")
LallN24_ready <- prepare_data(LallN24,
                             what_to_do_with_value = "nothing",
                             small_const_to_add = 0)
draw_plot(LallN24_ready, "Problem A, N=24", breaks=c(-2, -1, 0, 1, 2),
          y_label = "minimum value found", y_line_position = optimum_Lall_N24,
          label_line = "global minimum", add_tick_minimum = TRUE,
          leave_out_y_ticks = c(-1))
ggsave(filename = "results/plots/LallN24.png", width = 7, height = 4.5,
       dpi=700)

LallN24_ready <- prepare_data(LallN24, what_to_do_with_value = "make differences",
                             best_known_value = optimum_Lall_N24,
                             small_const_to_add = 1e-6)
draw_plot(LallN24_ready, "Problem A, N=24", breaks=c(-3, -2, -1, 0, 1, 2),
          y_label = "log10 of (difference from the expected global minimum + 1e-6)",
          y_line_position = -6, label_line = "zero difference")
ggsave(filename = "results/plots/LallN24_2.png", width = 7, height = 4.5,
       dpi=700)

LallN96 <- read.csv("results/LallN96.csv")
LallN96_ready <- prepare_data(LallN96,
                              what_to_do_with_value = "nothing",
                              small_const_to_add = 0)
draw_plot(LallN96_ready, "Problem A, N=96", breaks=c(-2, -1, 0, 1, 2),
          y_label = "minimum value found", y_line_position = optimum_Lall_N96,
          label_line = "global minimum", add_tick_minimum = TRUE)
ggsave(filename = "results/plots/LallN96.png", width = 7, height = 4.5,
       dpi=700)

LallN96_ready <- prepare_data(LallN96, what_to_do_with_value = "make differences",
                              best_known_value = optimum_Lall_N96,
                              small_const_to_add = 1e-6)
draw_plot(LallN96_ready, "Problem A, N=96", breaks=c(-2, -1, 0, 1, 2),
          y_label = "log10 of (difference from the expected global minimum + 1e-6)",
          y_line_position = -6, label_line = "zero difference",
          custom_y_breaks = c(0, -1, -2, -3, -4, -5, -6))
ggsave(filename = "results/plots/LallN96_2.png", width = 7, height = 4.5,
       dpi=700)



DuarteN10 <- read.csv("results/DuarteN10.csv")
DuarteN10_ready <- prepare_data(DuarteN10,
                                what_to_do_with_value = "nothing",
                                small_const_to_add = 0)
draw_plot(DuarteN10_ready, "Problem B, N=10", breaks=c(-2, -1, 0, 1, 2),
          y_label = "minimum value found", y_line_position = optimum_Duarte_N10,
          label_line = "global minimum", add_tick_minimum = TRUE,
          leave_out_y_ticks = c(-20))
ggsave(filename = "results/plots/DuarteN10.png", width = 7, height = 4.5,
       dpi=700)

DuarteN10_ready <- prepare_data(DuarteN10,
                                what_to_do_with_value = "make differences",
                                best_known_value = optimum_Duarte_N10,
                                small_const_to_add = 1e-6)
draw_plot(DuarteN10_ready, "Problem B, N=10", breaks=c(-2, -1, 0, 1, 2),
          y_label = "log10 of (difference from the expected global minimum + 1e-6)",
          y_line_position = -6, label_line = "zero difference",
          custom_y_breaks = c(2, 1, 0, -1, -2, -3, -4, -5, -6))
ggsave(filename = "results/plots/DuarteN10_2.png", width = 7, height = 4.5,
       dpi=700)


DuarteN50 <- read.csv("results/DuarteN50.csv")
DuarteN50_ready <- prepare_data(DuarteN50,
                                what_to_do_with_value = "nothing",
                                small_const_to_add = 0)
draw_plot(DuarteN50_ready, "Problem B, N=50", breaks=c(-2, -1, 0, 1, 2),
          y_label = "minimum value found", y_line_position = optimum_Duarte_N50,
          label_line = "global minimum", add_tick_minimum = TRUE,
          leave_out_y_ticks = c(-38))
ggsave(filename = "results/plots/DuarteN50.png", width = 7, height = 4.5,
       dpi=700)

DuarteN50_ready <- prepare_data(DuarteN50,
                                what_to_do_with_value = "make differences",
                                best_known_value = optimum_Duarte_N50,
                                small_const_to_add = 1e-6)
draw_plot(DuarteN50_ready, "Problem B, N=50", breaks=c(-2, -1, 0, 1, 2),
          y_label = "log10 of (difference from the expected global minimum + 1e-6)",
          y_line_position = -6, label_line = "zero difference",
          custom_y_breaks = c(2, 1, 0, -1, -2, -3, -4, -5, -6))
ggsave(filename = "results/plots/DuarteN50_2.png", width = 7, height = 4.5,
       dpi=700)


DuarteN100 <- read.csv("results/DuarteN100.csv")
DuarteN100_ready <- prepare_data(DuarteN100,
                                what_to_do_with_value = "nothing",
                                small_const_to_add = 0)
draw_plot(DuarteN100_ready, "Problem B, N=100", breaks=c(-2, -1, 0, 1, 2),
          y_label = "minimum value found", y_line_position = optimum_Duarte_N100,
          label_line = "global minimum", add_tick_minimum = TRUE,
          leave_out_y_ticks = c(-38))
ggsave(filename = "results/plots/DuarteN100.png", width = 7, height = 4.5,
       dpi=700)

DuarteN100_ready <- prepare_data(DuarteN100,
                                 what_to_do_with_value = "make differences",
                                 best_known_value = optimum_Duarte_N100,
                                 small_const_to_add = 1e-6)
draw_plot(DuarteN100_ready, "Problem B, N=100", breaks=c(-2, -1, 0, 1, 2),
          y_label = "log10 of (difference from the expected global minimum + 1e-6)",
          y_line_position = -6, label_line = "zero difference",
          custom_y_breaks = c(2, 1, 0, -1, -2, -3, -4, -5, -6))
ggsave(filename = "results/plots/DuarteN100_2.png", width = 7, height = 4.5,
       dpi=700)


DuarteN200 <- read.csv("results/DuarteN200.csv")
DuarteN200_ready <- prepare_data(DuarteN200,
                                 what_to_do_with_value = "nothing",
                                 small_const_to_add = 0)
draw_plot(DuarteN200_ready, "Problem B, N=200", breaks=c(-2, -1, 0, 1, 2),
          y_label = "minimum value found", y_line_position = optimum_Duarte_N200,
          label_line = "global minimum", add_tick_minimum = TRUE,
          leave_out_y_ticks = c(-52))
ggsave(filename = "results/plots/DuarteN200.png", width = 7, height = 4.5,
       dpi=700)

DuarteN200_ready <- prepare_data(DuarteN200,
                                 what_to_do_with_value = "make differences",
                                 best_known_value = optimum_Duarte_N200,
                                 small_const_to_add = 1e-6)
draw_plot(DuarteN200_ready, "Problem B, N=200", breaks=c(-2, -1, 0, 1, 2),
          y_label = "log10 of (difference from the expected global minimum + 1e-6)",
          y_line_position = -6, label_line = "zero difference",
          custom_y_breaks = c(1, 0, -1, -2, -3, -4, -5, -6))
ggsave(filename = "results/plots/DuarteN200_2.png", width = 7, height = 4.5,
       dpi=700)
