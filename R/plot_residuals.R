#' Residual diagnostic plots for a fitted model
#'
#' @name mysterycall_plot_residuals
NULL

#' Three-panel residual diagnostic plot
#'
#' Returns a named list of three ggplot2 plots: residuals vs. fitted values,
#' a normal Q-Q plot of Pearson residuals, and a scale-location plot. Works
#' with [mysterycall_poisson_model()] results, `glm`, `glmerMod`, or `lm`
#' objects.
#'
#' @param model A `mysterycall_poisson_model` result, or a `glm`, `glmerMod`,
#'   or `lm` object.
#'
#' @return A named list with elements `residuals_vs_fitted`, `qq`, and
#'   `scale_location`, each a `ggplot` object.
#'
#' @family outcomes
#' @export
#'
#' @examples
#' m <- glm(mpg ~ wt, data = mtcars, family = gaussian())
#' plots <- mysterycall_plot_residuals(m)
#' plots$qq
mysterycall_plot_residuals <- function(model) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is required for this function.", call. = FALSE)
  }

  if (inherits(model, "mysterycall_poisson_model")) {
    fit <- model$model
  } else if (inherits(model, c("glmerMod", "glm", "lm"))) {
    fit <- model
  } else {
    stop(
      "`model` must be a `mysterycall_poisson_model`, glm, glmerMod, or lm object.",
      call. = FALSE
    )
  }

  fitted_vals    <- as.numeric(fitted(fit))
  resid_vals     <- as.numeric(residuals(fit, type = "pearson"))
  sqrt_abs_resid <- sqrt(abs(resid_vals))

  df_plot <- data.frame(
    fitted         = fitted_vals,
    residuals      = resid_vals,
    sqrt_abs_resid = sqrt_abs_resid
  )

  p1 <- ggplot2::ggplot(df_plot, ggplot2::aes(x = fitted, y = residuals)) +
    ggplot2::geom_point(alpha = 0.5) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    ggplot2::labs(
      title = "Residuals vs Fitted",
      x     = "Fitted values",
      y     = "Pearson residuals"
    ) +
    ggplot2::theme_minimal()

  p2 <- ggplot2::ggplot(df_plot, ggplot2::aes(sample = residuals)) +
    ggplot2::stat_qq() +
    ggplot2::stat_qq_line(color = "red") +
    ggplot2::labs(
      title = "Normal Q-Q",
      x     = "Theoretical quantiles",
      y     = "Sample quantiles"
    ) +
    ggplot2::theme_minimal()

  p3 <- ggplot2::ggplot(df_plot, ggplot2::aes(x = fitted, y = sqrt_abs_resid)) +
    ggplot2::geom_point(alpha = 0.5) +
    ggplot2::geom_smooth(method = "loess", se = FALSE, color = "red",
                         formula = y ~ x) +
    ggplot2::labs(
      title = "Scale-Location",
      x     = "Fitted values",
      y     = "sqrt(|Pearson residuals|)"
    ) +
    ggplot2::theme_minimal()

  list(residuals_vs_fitted = p1, qq = p2, scale_location = p3)
}
