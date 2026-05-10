#' Compute MAE and RMSE from a fitted Poisson or linear model
#'
#' @name mysterycall_model_metrics
NULL

#' Compute MAE and RMSE from a fitted model
#'
#' Extracts fitted values and the observed response from a fitted model and
#' returns mean absolute error and root-mean-square error. Works with
#' [mysterycall_poisson_model()] results, bare `glm`, `glmerMod`, or `lm`
#' objects.
#'
#' @param model A `mysterycall_poisson_model` result, or a `glm`, `glmerMod`,
#'   or `lm` object.
#'
#' @return A named list with elements `mae` and `rmse` (numeric scalars).
#'
#' @family outcomes
#' @export
#'
#' @examples
#' m <- glm(mpg ~ wt, data = mtcars, family = gaussian())
#' mysterycall_model_metrics(m)
mysterycall_model_metrics <- function(model) {
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

  obs  <- as.numeric(model.response(model.frame(fit)))
  pred <- as.numeric(fitted(fit))

  mae  <- mean(abs(obs - pred), na.rm = TRUE)
  rmse <- sqrt(mean((obs - pred)^2L, na.rm = TRUE))

  list(mae = mae, rmse = rmse)
}
