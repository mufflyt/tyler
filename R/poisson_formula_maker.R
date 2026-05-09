#' Create a Formula for Poisson Model
#'
#' This function creates a formula for a Poisson model based on the provided data,
#' response variable, and optional random effect.
#'
#' @param data A dataframe containing the predictor and response variables.
#' @param response_var The name of the response variable in the dataframe.
#' @param random_effect Optional. The name of the random effect variable for the formula.
#'
#' @return A formula object suitable for modeling in R.
#' @importFrom stats as.formula
#'
#' @examples
#' # Example usage:
#' response_variable <- "days"
#' random_effect_term <- "name"  # Change this to the desired random effect variable
#' df3_filtered <- data.frame(days = c(5, 10, 15), age = c(30, 40, 50), name = c("A", "B", "C"))
#' formula <- tyler_create_formula(df3_filtered, response_variable, random_effect_term)
#' formula
#'
#' @family modeling helpers
#' @export
tyler_create_formula <- function(data, response_var, random_effect = NULL) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }
  if (!response_var %in% names(data)) {
    stop(sprintf("Response variable '%s' not found in data.", response_var), call. = FALSE)
  }
  if (!is.null(random_effect) && !random_effect %in% names(data)) {
    stop(sprintf("Random effect variable '%s' not found in data.", random_effect), call. = FALSE)
  }

  message(sprintf("Creating formula with response variable: %s", response_var))

  # Get the column names of the dataframe except for the response variable and random effect
  predictor_vars <- setdiff(names(data), c(response_var, random_effect))

  if (length(predictor_vars) == 0) {
    stop("No predictor variables remain after excluding response_var and random_effect.", call. = FALSE)
  }
  message(sprintf(
    "Predictor variables identified: %s",
    paste(predictor_vars, collapse = ", ")
  ))

  # Enclose predictor variables in backticks to handle special characters or spaces
  predictor_vars <- sapply(predictor_vars, function(x) paste0("`", x, "`"))
  message(sprintf(
    "Predictor variables after formatting: %s",
    paste(predictor_vars, collapse = ", ")
  ))

  # Create the initial formula string without the random effect
  formula_str <- paste(response_var, "~", paste(predictor_vars, collapse = " + "))
  message(sprintf("Initial formula string: %s", formula_str))

  # If a random effect is provided, append it to the formula
  if (!is.null(random_effect)) {
    formula_str <- paste(formula_str, "+ (1 |", random_effect, ")")
    message(sprintf("Formula string with random effect: %s", formula_str))
  }

  # Convert the formula string to a formula object
  formula_obj <- as.formula(formula_str)
  message(sprintf("Final formula object created: %s", deparse(formula_obj)))

  return(formula_obj)
}
