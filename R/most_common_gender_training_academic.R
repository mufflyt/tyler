#' Generate a Summary Sentence for the Most Common Gender, Specialty, Training, and Academic Affiliation
#'
#' This function calculates and returns a sentence that describes the most common gender, specialty, training, and academic affiliation in the provided dataset.
#'
#' @param df A data frame containing the columns `gender`, `specialty`, `Provider.Credential.Text`, and `academic_affiliation`.
#'
#' @return A character string summarizing the most common gender, specialty, training, and academic affiliation along with their respective proportions.
#'
#' @details The function filters out missing values in each column before determining the most common value. It then calculates the proportion of this most common value relative to the total non-missing values in that column.
#'
#' @examples
#' # Example 1: Basic usage with a small dataset
#' df <- data.frame(
#'   gender = c("Male", "Female", "Female", "Male", "Male"),
#'   specialty = c("Cardiology", "Cardiology", "Neurology", "Cardiology", "Neurology"),
#'   Provider.Credential.Text = c("MD", "MD", "DO", "MD", "DO"),
#'   academic_affiliation = c("Yes", "No", "Yes", "No", "Yes")
#' )
#' result <- most_common_gender_training_academic(df)
#' print(result)
#'
#' # Example 2: Handling missing data
#' df_with_na <- data.frame(
#'   gender = c("Male", NA, "Female", "Male", "Male"),
#'   specialty = c("Cardiology", "Cardiology", "Neurology", NA, "Neurology"),
#'   Provider.Credential.Text = c("MD", "MD", "DO", "MD", "DO"),
#'   academic_affiliation = c("Yes", "No", "Yes", "No", NA)
#' )
#' result <- most_common_gender_training_academic(df_with_na)
#' print(result)
#'
#' # Example 3: Different proportions with a larger dataset
#' df_large <- data.frame(
#'   gender = c(rep("Male", 70), rep("Female", 30)),
#'   specialty = c(rep("Cardiology", 50), rep("Neurology", 30), rep("Orthopedics", 20)),
#'   Provider.Credential.Text = c(rep("MD", 60), rep("DO", 40)),
#'   academic_affiliation = c(rep("Yes", 40), rep("No", 60))
#' )
#' result <- most_common_gender_training_academic(df_large)
#' print(result)
#'
#' @import dplyr
#' @export
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
