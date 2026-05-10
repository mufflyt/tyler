#' Distribution plots for numeric outcome variables
#'
#' @name mysterycall_plot_distribution
NULL

#' Two-panel histogram: raw count and sqrt-transformed count
#'
#' Returns two ggplot2 histograms side-by-side as a named list. The first
#' panel shows raw counts; the second applies a square-root y-axis transform,
#' which makes right-skewed count data easier to read.
#'
#' @param x Numeric vector to visualise.
#' @param title Character scalar used as the base plot title. If `NULL`
#'   (default), the deparsed expression of `x` is used.
#' @param bins Integer number of histogram bins. Default `30L`.
#'
#' @return A named list with elements `raw` and `sqrt_transformed`, each a
#'   `ggplot` object.
#'
#' @family outcomes
#' @export
#'
#' @examples
#' plots <- mysterycall_plot_distribution(rpois(200, 14), title = "Wait days")
#' plots$raw
mysterycall_plot_distribution <- function(x, title = NULL, bins = 30L) {
  if (!is.numeric(x)) stop("`x` must be a numeric vector.", call. = FALSE)
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is required.", call. = FALSE)
  }

  main_title <- if (!is.null(title)) title else deparse(substitute(x))
  df <- data.frame(value = x[!is.na(x)])

  p1 <- ggplot2::ggplot(df, ggplot2::aes(x = value)) +
    ggplot2::geom_histogram(bins = bins, fill = "#2C3E50", color = "white") +
    ggplot2::labs(title = paste0(main_title, " (raw)"), x = NULL, y = "Count") +
    ggplot2::theme_minimal()

  p2 <- ggplot2::ggplot(df, ggplot2::aes(x = value)) +
    ggplot2::geom_histogram(bins = bins, fill = "#2C3E50", color = "white") +
    ggplot2::scale_y_sqrt() +
    ggplot2::labs(
      title = paste0(main_title, " (sqrt-scaled count)"),
      x     = NULL,
      y     = "sqrt(Count)"
    ) +
    ggplot2::theme_minimal()

  list(raw = p1, sqrt_transformed = p2)
}
