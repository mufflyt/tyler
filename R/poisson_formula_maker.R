#' Create a Formula for Poisson Model
#'
#' This function creates a formula for a Poisson model based on the provided data,
#' response variable, and optional random effect.
#'
#' @param data A dataframe containing the predictor and response variables.
#' @param response_var The name of the response variable in the dataframe.
#' @param random_effect Optional. The name of the random effect variable for the formula.
#' @param verbose Logical; if TRUE, prints status messages while running. Default is FALSE.
#'
#' @return A formula object suitable for modeling in R.
#' @importFrom stats as.formula
#'
#' @examples
#' # Example usage:
#' response_variable <- "days"
#' random_effect_term <- "name"  # Change this to the desired random effect variable
#' df3_filtered <- data.frame(days = c(5, 10, 15), age = c(30, 40, 50), name = c("A", "B", "C"))
#' formula <- create_formula(df3_filtered, response_variable, random_effect_term)
#' formula
#'
#' @export
create_formula <- function(data, response_var, random_effect = NULL, verbose = FALSE) {
  if (isTRUE(verbose)) {
    message("Creating formula with response variable: ", response_var)
  }

  # Get the column names of the dataframe except for the response variable
  predictor_vars <- setdiff(names(data), response_var)
  if (isTRUE(verbose)) {
    message("Predictor variables identified: ", paste(predictor_vars, collapse = ", "))
  }

  # Enclose predictor variables in backticks to handle special characters or spaces
  predictor_vars <- sapply(predictor_vars, function(x) paste0("`", x, "`"))
  if (isTRUE(verbose)) {
    message("Predictor variables after formatting: ", paste(predictor_vars, collapse = ", "))
  }

  # Create the initial formula string without the random effect
  formula_str <- paste(response_var, "~", paste(predictor_vars, collapse = " + "))
  if (isTRUE(verbose)) {
    message("Initial formula string: ", formula_str)
  }

  # If a random effect is provided, append it to the formula
  if (!is.null(random_effect)) {
    formula_str <- paste(formula_str, "+ (1 |", random_effect, ")")
    if (isTRUE(verbose)) {
      message("Formula string with random effect: ", formula_str)
    }
  }

  # Convert the formula string to a formula object
  formula_obj <- as.formula(formula_str)
  if (isTRUE(verbose)) {
    message("Final formula object created:")
    print(formula_obj)
  }

  return(formula_obj)
}
