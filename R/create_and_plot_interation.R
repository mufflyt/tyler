#' Create and Plot Interaction Effects in GLMM
#'
#' This function reads data from a specified file, fits a generalized linear mixed model (GLMM) with a specified interaction term, and creates a plot to visualize the interaction effects. The plot is saved to a specified directory. This is particularly useful for analyzing how two categorical variables interact to affect a response variable, controlling for a random intercept.
#'
#' @param data_path A character string specifying the path to the .rds file containing the dataset.
#' @param response_variable A character string specifying the name of the response variable in the dataset.
#' @param variable_of_interest A character string specifying the first categorical predictor variable in the interaction.
#' @param interaction_variable A character string specifying the second categorical predictor variable in the interaction.
#' @param random_intercept A character string specifying the variable to be used as the random intercept in the model (e.g., "city").
#' @param output_path A character string specifying the directory where the interaction plot will be saved.
#' @param resolution An integer specifying the resolution (in DPI) for saving the plot. Defaults to 100.
#'
#' @return A list containing the fitted GLMM (`model`) and the summarized data used for the effects plot (`effects_plot_data`).
#'
#' @details The function performs the following steps:
#' - Reads the data from the provided file path.
#' - Renames and converts the specified columns to appropriate types.
#' - Fits a Poisson GLMM with an interaction term and a random intercept.
#' - Summarizes the interaction effects and creates a plot.
#' - Saves the plot to the specified directory.
#'
#' This function is useful in scenarios where you want to examine the interaction between two categorical variables and their combined effect on a count-based response variable (e.g., the number of business days until an appointment). It can handle complex data structures and controls for variability across groups using random intercepts.
#'
#' @examples
#' \dontrun{
#' # Example 1: Analyzing the effect of gender and appointment center on wait times
#' result <- create_and_plot_interaction(
#'   data_path = "Ari/data/Phase2/late_phase_2_ENT_analysis_3.rds",
#'   response_variable = "business_days_until_appointment",
#'   variable_of_interest = "central_number_e_g_appointment_center",
#'   interaction_variable = "gender",
#'   random_intercept = "city",
#'   output_path = "Ari/data/figures",
#'   resolution = 100
#' )
#'
#' # Example 2: Examining the interaction between insurance type and scenario on appointment delays
#' result <- create_and_plot_interaction(
#'   data_path = "data/healthcare_calls.rds",
#'   response_variable = "business_days_until_appointment",
#'   variable_of_interest = "insurance_type",
#'   interaction_variable = "scenario",
#'   random_intercept = "state",
#'   output_path = "figures/insurance_scenario_interaction",
#'   resolution = 150
#' )
#'
#' # Example 3: Studying the interaction between gender and subspecialty in wait times
#' result <- create_and_plot_interaction(
#'   data_path = "data/mystery_caller_study.rds",
#'   response_variable = "waiting_time_days",
#'   variable_of_interest = "subspecialty",
#'   interaction_variable = "gender",
#'   random_intercept = "clinic_id",
#'   output_path = "results/waiting_times",
#'   resolution = 300
#' )
#' }
#'
#' @import lme4 dplyr ggplot2
#' @export
create_and_plot_interaction <- function(data_path, response_variable, variable_of_interest, interaction_variable, random_intercept, output_path, resolution = 100) {
  # Read the data
  data <- readRDS(data_path)

  # Ensure the data is a data frame
  if (!is.data.frame(data)) {
    stop("Data must be a data.frame")
  }

  # Log inputs
  cat("Inputs:\n")
  cat("response_variable:", response_variable, "\n")
  cat("variable_of_interest:", variable_of_interest, "\n")
  cat("interaction_variable:", interaction_variable, "\n")
  cat("random_intercept:", random_intercept, "\n")
  cat("output_path:", output_path, "\n")
  cat("resolution:", resolution, "\n\n")

  # Rename the columns for simplicity
  data <- data %>%
    dplyr::rename(
      response_var = !!rlang::sym(response_variable),
      var_interest = !!rlang::sym(variable_of_interest),
      int_var = !!rlang::sym(interaction_variable)
    )

  # Ensure the columns are in the correct format
  data <- data %>%
    dplyr::mutate(
      response_var = as.numeric(response_var),
      var_interest = as.factor(var_interest),
      int_var = as.factor(int_var)
    )

  # Remove any rows with NA values
  data <- na.omit(data)

  # Log the first few rows of data to check formats
  cat("Data preview after renaming and type conversion:\n")
  print(head(data))
  cat("\n\n")

  # Construct the model formula
  interaction_term <- "int_var * var_interest"
  model_formula <- as.formula(paste("response_var ~", interaction_term, "+ (1 |", random_intercept, ")"))

  # Log model formula
  cat("Model formula:", deparse(model_formula), "\n\n")

  # Fit the model with interaction
  cat("Fitting the model...\n")
  glmer_model <- lme4::glmer(model_formula,
                             data = data,
                             family = poisson(link = "log"),
                             nAGQ = 0,
                             verbose = 0L)
  cat("Model fitted successfully.\n\n")

  # Log model summary
  cat("Model summary:\n")
  print(summary(glmer_model))
  cat("\n\n")

  # Create the effects plot manually
  cat("Creating effects plot...\n")
  pred_data <- data %>%
    dplyr::mutate(pred = predict(glmer_model, type = "response"))

  plot_data <- pred_data %>%
    group_by(int_var, var_interest) %>%
    summarise(mean_pred = mean(pred), .groups = 'drop')

  ggplot(plot_data, aes(x = int_var, y = mean_pred, color = var_interest)) +
    geom_point() +
    geom_line(aes(group = var_interest)) +
    labs(title = "Interaction Effect Plot", y = response_variable, x = interaction_variable) +
    theme_minimal()

  # Save the plot
  plot_filename <- file.path(output_path, paste0("interaction_", interaction_variable, "_", variable_of_interest, ".png"))
  cat("Saving effects plot to:", plot_filename, "\n")
  ggsave(plot_filename, width = 6, height = 4, dpi = resolution)
  cat("Effects plot saved successfully.\n\n")

  # Log outputs
  cat("Outputs:\n")
  print(glmer_model)
  print("Effects plot object:\n")
  print(plot_data)

  # Return the model and effects plot data
  return(list(model = glmer_model, effects_plot_data = plot_data))
}
