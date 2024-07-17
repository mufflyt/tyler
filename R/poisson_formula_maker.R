# Mystery caller formula creation for Poisson Model
create_formula <- function(data, response_var, random_effect = NULL) {
  # Get the column names of the dataframe except for the response variable
  predictor_vars <- setdiff(names(data), response_var)
  
  # Enclose predictor variables in backticks to handle special characters or spaces
  predictor_vars <- paste("`", predictor_vars, "`", sep = "")
  
  # Create the initial formula string without the random effect
  formula_str <- paste(response_var, "~", paste(predictor_vars, collapse = " + "))
  
  # If a random effect is provided, append it to the formula
  if (!is.null(random_effect)) {
    formula_str <- paste(formula_str, "+ (1 |", random_effect, ")")
  }
  
  # Convert the formula string to a formula object
  formula_obj <- as.formula(formula_str)
  
  return(formula_obj)
}

# Use case:  
# Call the function with your dataframe, response variable, and random effect
# response_variable <- "days"
# random_effect_term <- "name"  # Change this to the desired random effect variable
# formula <- create_formula(df3_filtered, response_variable, random_effect_term)
# 
# # Print the created formula
# formula 