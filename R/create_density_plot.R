#' Create a Density Plot for Mystery Caller Studies with Optional Transformations
#'
#' This function generates a density plot designed for mystery caller studies, allowing for the visualization of waiting times or similar outcomes across different categories, such as insurance types. The function supports transformations on the x-axis and custom labels.
#'
#' @param data A dataframe containing the data to be plotted. Must contain the variables specified in `x_var` and `fill_var`.
#' @param x_var A string representing the column name for the x-axis variable. This should be a numeric variable (e.g., waiting time in days).
#' @param fill_var A string representing the column name for the fill variable. This should be a categorical or factor variable (e.g., insurance type).
#' @param x_transform A string specifying the transformation for the x-axis: "log" for log transformation (log1p), "sqrt" for square root transformation, or "none" for no transformation. Default is "none".
#' @param dpi An integer specifying the resolution of the saved plot in dots per inch (DPI). Default is 100.
#' @param output_dir A string representing the directory where the plot files will be saved. Default is "output".
#' @param file_prefix A string used as the prefix for the generated plot filenames. The filenames will have a timestamp appended to ensure uniqueness. Default is "density_plot".
#' @param x_label A string specifying the label for the x-axis. Default is `NULL` (uses x_var).
#' @param y_label A string specifying the label for the y-axis. Default is "Density".
#' @param plot_title A string specifying the title of the plot. Default is `NULL` (no title).
#' @param verbose A boolean indicating whether to print messages about the saved plot locations. Default is TRUE.
#'
#' @return Invisibly returns the generated ggplot object.
#' @importFrom dplyr filter mutate %>%
#' @importFrom ggplot2 ggplot geom_density scale_x_log10 scale_x_sqrt labs theme_light theme ggsave
#' @importFrom viridis viridis_pal
#' @importFrom rlang sym .data
#' @family mapping
#' @export
#' @examples
#' \dontrun{
#' # Example 1: Basic density plot with log transformation
#' create_density_plot(
#'     data = df3,
#'     x_var = "business_days_until_appointment",
#'     fill_var = "insurance",
#'     x_transform = "log",  # Log transformation
#'     dpi = 100,
#'     output_dir = "ortho_sports_med/Figures",
#'     file_prefix = "ortho_sports_vs_insurance_density",
#'     x_label = "Log (Waiting Times in Days)",
#'     y_label = "Density",
#'     plot_title = "Density Plot of Waiting Times by Insurance"
#' )
#'
#' # Example 2: Density plot with square root transformation
#' create_density_plot(
#'     data = df3,
#'     x_var = "business_days_until_appointment",
#'     fill_var = "insurance",
#'     x_transform = "sqrt",  # Square root transformation
#'     dpi = 150,
#'     output_dir = "ortho_sports_med/Figures",
#'     file_prefix = "ortho_sports_vs_insurance_density_sqrt",
#'     x_label = "Sqrt (Waiting Times in Days)",
#'     y_label = "Density",
#'     plot_title = "Square Root Transformed Density Plot"
#' )
#'
#' # Example 3: Density plot without any transformation
#' create_density_plot(
#'     data = df3,
#'     x_var = "business_days_until_appointment",
#'     fill_var = "insurance",
#'     x_transform = "none",  # No transformation
#'     dpi = 200,
#'     output_dir = "ortho_sports_med/Figures",
#'     file_prefix = "ortho_sports_vs_insurance_density_none",
#'     x_label = "Waiting Times in Days",
#'     y_label = "Density",
#'     plot_title = "Density Plot Without Transformation"
#' )
#' }

create_density_plot <- function(data,
                                x_var,
                                fill_var,
                                x_transform = "none",
                                dpi = 100,
                                output_dir = "output",
                                file_prefix = "density_plot",
                                x_label = NULL,
                                y_label = "Density",
                                plot_title = NULL,
                                verbose = TRUE) {

  # Filter out zero or negative values and NAs from the x_var column
  data <- dplyr::filter(data, .data[[x_var]] > 0, !is.na(.data[[x_var]]))

  # Handle transformations
  if (x_transform == "log") {
    data <- dplyr::mutate(data, !!x_var := log1p(.data[[x_var]]))
    x_label <- if (is.null(x_label)) paste("Log (", x_var, ")", sep = "") else x_label
  } else if (x_transform == "sqrt") {
    data <- dplyr::mutate(data, !!x_var := sqrt(.data[[x_var]]))
    x_label <- if (is.null(x_label)) paste("Sqrt (", x_var, ")", sep = "") else x_label
  } else {
    x_label <- if (is.null(x_label)) x_var else x_label
  }

  # Create the density plot
  density_plot <- ggplot2::ggplot(data, ggplot2::aes(x = !!rlang::sym(x_var), fill = !!rlang::sym(fill_var))) +
    ggplot2::geom_density(alpha = 0.5) +
    ggplot2::labs(
      x = x_label,
      y = y_label,
      title = plot_title
    ) +
    ggplot2::scale_fill_viridis_d() +  # Use viridis color palette
    ggplot2::theme_light() +
    ggplot2::theme(
      legend.position = "bottom"
    )

  # Display the plot
  print(density_plot)

  # Automatic Filename Generation
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  tiff_filename <- file.path(output_dir, paste0(file_prefix, "_", timestamp, ".tiff"))
  png_filename <- file.path(output_dir, paste0(file_prefix, "_", timestamp, ".png"))

  ggplot2::ggsave(filename = tiff_filename, plot = density_plot, dpi = dpi, height = 8, width = 11)
  ggplot2::ggsave(filename = png_filename, plot = density_plot, dpi = dpi, height = 8, width = 11)

  if (verbose) {
    cat("Plots saved to:", tiff_filename, "and", png_filename, "\n")
  }

  invisible(density_plot)
}
