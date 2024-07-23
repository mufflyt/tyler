library(ggplot2)
library(emmeans)
library(dplyr)

plot_and_save_emmeans <- function(model_object, specs, variable_of_interest, output_dir = "ortho/Figures",
                                  width = 10, height = 6, ylim_bounds = c(10, 30),
                                  point_size = 2, point_shape = 19, error_width = 0.2,
                                  position_dodge_width = 0.2, title_prefix = "Estimated Marginal Means -",
                                  y_label = "Estimated Marginal Means for Waiting Time in Days\n Mean ± 95% CI") {

  # Parameter validation
  if (!inherits(model_object, "lm")) { # Adjust "lm" to the specific model classes expected
    stop("model_object must be a valid model class object.")
  }
  if (!is.character(specs) || length(specs) != 1) {
    stop("specs must be a single character string specifying the model terms.")
  }
  if (!is.character(variable_of_interest) || length(variable_of_interest) != 1) {
    stop("variable_of_interest must be a single character string.")
  }
  if (!dir.exists(output_dir)) {
    stop("Output directory does not exist: ", output_dir)
  }

  message("Starting the calculation of estimated marginal means.")

  # Calculate emmeans and convert to dataframe
  edata <- tryCatch({
    emmeans::emmeans(object = model_object, specs = specs, type = "response") %>%
      as.data.frame()
  }, error = function(e) {
    stop("Error in calculating emmeans: ", e$message)
  })

  message("Processing data for plotting.")

  # Check for the presence of the 'insurance' column
  if ("insurance" %in% names(edata)) {
    color_aesthetic <- aes(color = insurance)
  } else {
    color_aesthetic <- aes(color = "black") # Default color if 'insurance' is absent
    warning("The 'insurance' column was not found in the data. Plotting without color differentiation by insurance.")
  }

  # Check and set y-limits dynamically if not specified
  if (is.null(ylim_bounds)) {
    ylim_bounds <- range(c(edata$asymp.LCL, edata$asymp.UCL), na.rm = TRUE)
    ylim_bounds <- ylim_bounds + c(-1, 1) * diff(ylim_bounds) * 0.1 # Adding 10% padding
    message("Y-axis limits set dynamically based on the data.")
  }

  # Plot creation
  message("Creating the plot.")
  p <- ggplot(edata, aes(x = .data[[variable_of_interest]], y = "rate", color = .data[[variable_of_interest]])) +
    geom_point(size = point_size, shape = point_shape, position = position_dodge(width = position_dodge_width)) +
    geom_errorbar(aes(ymin = "asymp.LCL", ymax = "asymp.UCL"), width = error_width, position = position_dodge(width = position_dodge_width)) +
    ylim(ylim_bounds) +
    ggtitle(paste(title_prefix, variable_of_interest)) +
    ylab(y_label) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 10, color = "black"), legend.position = "bottom") +
    scale_x_discrete(labels = function(x) gsub(" ", "\n", x))

  # Optionally set color scheme
  if (!is.null(color_aesthetic)) {
    p <- p + scale_color_manual(values = color_aesthetic)
  }

  # File path and save plot
  message("Saving the plot.")
  file_name <- paste0(output_dir, "/insuranceinteraction_", variable_of_interest, "_comparison_plot.png")
  ggsave(filename = file_name, plot = p, width = width, height = height, bg = "white")

  message("Plot saved successfully to ", file_name)

  # Return the data and plot
  return(list(data = edata, plot = p))
}
