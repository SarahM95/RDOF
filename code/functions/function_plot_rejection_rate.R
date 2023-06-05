#' Compact plot function 
#'
#' The rejection rate is plotted by several variables. 
#' The rejection rate by sample size for all methods by number of categories and probability distribution in the allocation variable.
#'
#'
#' @param data A data.frame containing the rejection rate and the corresponding sample size, number of categories, probability distribution, and method.
#' @param x character string. The x axis variable (sample size).
#' @param y character string. The y axis variable (rejection rate).
#' @param colour character string. The colour-coded variable in the plot (method).
#' @param xlab character string. The x-axis label.
#' @param ylab character string. The y-axis label.
#' @param facet_y character string. The row grouping.
#' @param facet_x character string. The column grouping.
#' @param yintercept A value indicating a possible horizontal line.
#' @param palette A data.frame containing the colours used for the colour-coded variable with the corresponding method.
#' @param xlim A value indicating the x-axis limits.
#' @param ylim A value indicating the y-axis limits.
#' @param smooth A boolean indicating whether the rejection rates are supposed to be smoothed over the methods.
#' @param scales character string. Is the y-axis "fixed" across all facets or "free"
#'
#'
#' @return A faceted plot of the rejection rates across sample sizes, numbers of categories and probability distributions.
#' @export
#'
#' @examples
#' 


plot_rejection_rate <- function(data,
                                x = "n_obs",
                                y = "rejection_rate",
                                colour = "method",
                                xlab = "Sample Size",
                                ylab = "Rejection Rate",
                                facet_y = "K",
                                facet_x = "treat_control_param", 
                                yintercept = NULL, 
                                palette = NULL, 
                                smooth = TRUE, 
                                scales = "fixed") {
  # Packages -----
  if (!require('data.table')) {
    install.packages('data.table')
    library('data.table')
  }
  if (!require('ggplot2')) {
    install.packages('ggplot2')
    library('ggplot2')
  }
  if (!require('ggthemes')) {
    install.packages('ggthemes')
    library('ggthemes')
  }
  
  
  # Making colour palette -----
  if (is.null(palette)) {
    mk.palette <- data.table()
    mk.palette$methods <-
      c(
        "Chi Squared (with correction)",
        "Mann Whitney (coin package)",
        "Cochran Armitage (coin package)",
        "Chi Squared",
        "Mann Whitney",
        "Fisher",
        "Stuarts tau c",
        "Cochran Armitage",
        "Logistic Regression",
        "Proportional Odds"
      )
    mk.palette$colours <-
      c(
        "#88BDE6",
        "#FBB258",
        "#BFA554",
        "#265DAB",
        "#DF5C24",
        "#059748",
        "#E5126F",
        "#9D722A",
        "#7B3A96",
        "#C7B42D"
      )
    
    palette <- mk.palette[methods %in% levels(as.factor(data$method))]
  } else{
    palette <- palette[methods %in% levels(as.factor(data$method))]
  }
  
c("#8C8C8C", "#88BDE6", "#FBB258", "#90CD97", "#F6AAC9", "#BFA554", "#A899C7", "#EDDD46", "#F07E6E")
c("#4D4D4D", "#5DA5DA", "#FAA43A", "#60BD68", "#F17CB0", "#B2912F", "#B276B2", "#DECF35", "#F15854")
c("#000000", "#265DAB", "#DF5C24", "#059748", "#E5126F", "#9D722A", "#7B3A96", "#C7B42D", "#CB2027")
  
  # Plotting rejection rate - line plot -----
if(smooth == TRUE) {
  ggplot(data,
         aes(x = .data[[x]],
             y = .data[[y]])) +
    geom_line(aes(colour = .data[[colour]]), alpha = 0.3) +
    geom_hline(yintercept = yintercept) +
    xlab(xlab) +
    ylab(ylab) +
    facet_grid(reformulate(facet_x, facet_y), scales = scales) +
    geom_smooth(linewidth = 0.6) +
    scale_color_manual(breaks = palette$methods, 
                       values = palette$colours, 
                       name = "Methods") +
    theme_tufte() +
    theme(axis.line = element_line(colour = "grey70"))
} else {
  ggplot(data,
         aes(x = .data[[x]],
             y = .data[[y]])) +
    geom_line(aes(colour = .data[[colour]])) +
    geom_hline(yintercept = yintercept) +
    xlab(xlab) +
    ylab(ylab) +
    facet_grid(reformulate(facet_x, facet_y), scales = scales) +
    scale_color_manual(breaks = palette$methods, 
                       values = palette$colours, 
                       name = "Methods") +
    theme_tufte() +
    theme(axis.line = element_line(colour = "grey70"))
}
  
}
