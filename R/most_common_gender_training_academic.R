library(dplyr)

most_common_gender_training_academic <- function(df) {
  # Helper function to get the most common value and proportion
  calculate_proportion <- function(df, column) {
    df <- df %>% filter(!is.na(!!sym(column)))
    total_count <- nrow(df)
    if (total_count == 0) {
      return(list(value = "", proportion = NaN))
    }
    most_common <- df %>%
      dplyr::count(!!sym(column), sort = TRUE) %>%
      dplyr::arrange(desc(n), !!sym(column)) %>%
      dplyr::slice(1)  # Select the first row after sorting by count and column
    proportion <- round((most_common$n / total_count) * 100, 1)
    list(value = most_common[[1]], proportion = proportion)
  }

  # Most common gender
  gender_info <- calculate_proportion(df, "gender")
  most_gender <- tolower(gender_info$value)
  proportion_gender <- gender_info$proportion

  # Most common specialty
  specialty_info <- calculate_proportion(df, "specialty")
  most_specialty <- specialty_info$value
  proportion_specialty <- specialty_info$proportion

  # Most common training
  training_info <- calculate_proportion(df, "Provider.Credential.Text")
  most_training <- training_info$value
  proportion_training <- training_info$proportion

  # Most common academic affiliation
  academic_info <- calculate_proportion(df, "academic_affiliation")
  most_academic <- tolower(academic_info$value)
  proportion_academic <- academic_info$proportion

  # Create the sentence
  sentence <- paste0(
    "The most common gender in the dataset was ", most_gender,
    " (", proportion_gender, "%). The most common specialty was ", most_specialty,
    " (", proportion_specialty, "%). The most common training was ", most_training,
    " (", proportion_training, "%). The academic affiliation status most frequently occurring was ", most_academic,
    " (", proportion_academic, "%)."
  )

  return(sentence)
}

# Example usage
# df <- data.frame(
#   gender = c("Male", "Female", "Female", "Male", "Male"),
#   specialty = c("Cardiology", "Cardiology", "Neurology", "Cardiology", "Neurology"),
#   Provider.Credential.Text = c("MD", "MD", "DO", "MD", "DO"),
#   academic_affiliation = c("Yes", "No", "Yes", "No", "Yes")
# )
# result <- most_common_gender_training_academic(df)
# print(result)
