#' Create a Line Plot with Optional Transformations and Grouping
#'
#' This function creates a line plot using ggplot2 with options for transforming the y-axis, grouping lines, and saving the plot with a specified resolution. The plot can be saved in both TIFF and PNG formats with automatic filename generation.
#'
#' @param data A dataframe containing the data to be plotted. Must contain the variables specified in `x_var` and `y_var`.
#' @param x_var A string representing the column name for the x-axis variable. This should be a categorical or factor variable.
#' @param y_var A string representing the column name for the y-axis variable. This should be a numeric variable.
#' @param y_transform A string specifying the transformation for the y-axis: "log" for log transformation (log1p), "sqrt" for square root transformation, or "none" for no transformation. Default is "none".
#' @param dpi An integer specifying the resolution of the saved plot in dots per inch (DPI). Default is 100.
#' @param output_dir A string representing the directory where the plot files will be saved. Default is "output".
#' @param file_prefix A string used as the prefix for the generated plot filenames. The filenames will have a timestamp appended to ensure uniqueness. Default is "line_plot".
#' @param use_geom_line A boolean indicating whether to include lines connecting points for grouped data. Default is FALSE.
#' @param geom_line_group A string representing the column name to group the lines by when `use_geom_line` is TRUE. This should be a categorical or factor variable.
#' @param point_color A string specifying the color of the points. Default is "viridis", which uses the viridis color palette.
#' @param line_color A string specifying the color of the summary line (median). Default is "red".
#' @param verbose A boolean indicating whether to print messages about the saved plot locations. Default is TRUE.
#'
#' @return This function saves the plot to the specified directory and returns no value.
#' @importFrom dplyr filter mutate %>%
#' @importFrom ggplot2 ggplot geom_point geom_line stat_summary ylab theme_minimal element_rect element_blank ggsave
#' @importFrom viridis viridis_pal
#' @importFrom rlang sym .data
#' @family mapping
#' @export
#' @examples
#' # Example 1: Basic plot with log transformation
#' create_line_plot(
#'     data = df3,
#'     x_var = "insurance",
#'     y_var = "business_days_until_appointment",
#'     y_transform = "log",  # Log transformation
#'     dpi = 100,
#'     output_dir = "ortho_sports_med/Figures",
#'     file_prefix = "ortho_sports_vs_insurance"
#' )
#'
#' # Example 2: Plot with square root transformation and lines grouped by 'last'
#' create_line_plot(
#'     data = df3,
#'     x_var = "insurance",
#'     y_var = "business_days_until_appointment",
#'     y_transform = "sqrt",  # Square root transformation
#'     dpi = 150,
#'     output_dir = "ortho_sports_med/Figures",
#'     file_prefix = "ortho_sports_vs_insurance_sqrt",
#'     use_geom_line = TRUE,  # Include lines
#'     geom_line_group = "last"  # Group lines by 'last' column
#' )
#'
#' # Example 3: Plot without any transformation and without lines
#' create_line_plot(
#'     data = df3,
#'     x_var = "insurance",
#'     y_var = "business_days_until_appointment",
#'     y_transform = "none",  # No transformation
#'     dpi = 200,
#'     output_dir = "ortho_sports_med/Figures",
#'     file_prefix = "ortho_sports_vs_insurance_none"
#' )

create_line_plot <- function(data,
                             x_var,
                             y_var,
                             y_transform = "none",
                             dpi = 100,
                             output_dir = "output",
                             file_prefix = "line_plot",
                             use_geom_line = FALSE,
                             geom_line_group = NULL,
                             point_color = "viridis",
                             line_color = "red",
                             verbose = TRUE) {

  # Remove NA values from the y_var column
  data <- dplyr::filter(data, !is.na(rlang::sym(y_var)))

  # Handle transformations
  if (y_transform == "log") {
    data <- dplyr::mutate(data, !!y_var := log1p(.data[[y_var]]))
    y_label <- paste("Log (", y_var, ")", sep = "")
  } else if (y_transform == "sqrt") {
    data <- dplyr::mutate(data, !!y_var := sqrt(.data[[y_var]]))
    y_label <- paste("Sqrt (", y_var, ")", sep = "")
  } else {
    y_label <- y_var
  }

  # Create the plot
  line_plot <- ggplot2::ggplot(data, ggplot2::aes(x = !!rlang::sym(x_var), y = !!rlang::sym(y_var))) +
    ggplot2::geom_point(color = ifelse(point_color == "viridis", viridis::viridis_pal()(1), point_color))

  if (use_geom_line & !is.null(geom_line_group)) {
    line_plot <- line_plot +
      ggplot2::geom_line(ggplot2::aes_string(group = geom_line_group), color = "gray80")
  }

  line_plot <- line_plot +
    ggplot2::stat_summary(fun = median, geom = "line", color = line_color, alpha = 0.7, size = 1, ggplot2::aes(group = 1)) +
    ggplot2::ylab(y_label) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = "white"),
      plot.background = ggplot2::element_rect(fill = "white"),
      axis.title.x = ggplot2::element_blank()
    )

  # Automatic Filename Generation
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  tiff_filename <- file.path(output_dir, paste0(file_prefix, "_", timestamp, ".tiff"))
  png_filename <- file.path(output_dir, paste0(file_prefix, "_", timestamp, ".png"))

  ggplot2::ggsave(filename = tiff_filename, plot = line_plot, dpi = dpi, height = 8, width = 11)
  ggplot2::ggsave(filename = png_filename, plot = line_plot, dpi = dpi, height = 8, width = 11)

  if (verbose) {
    cat("Plots saved to:", tiff_filename, "and", png_filename, "\n")
  }
}
