#' Genderize Physicians Data
#'
#' This function reads a CSV file containing physician data, genderizes the first names,
#' and joins the gender information back to the original data. It then saves the
#' result to a new CSV file with a timestamp.
#'
#' @param input_csv The path to the input CSV file containing physician data.
#' @param output_dir The directory where the output CSV file will be saved. Default is the current working directory.
#' @return A data frame with genderized information joined to the original data.
#'
#' @importFrom gender gender
#' @importFrom dplyr select filter rename distinct left_join
#' @importFrom readr read_csv write_csv
#'
#' @examples
#' \dontrun{
#' result <- genderize_physicians("sample.csv")
#' }
#'
#' @export
#' @seealso tyler
genderize_physicians <- function(input_csv, output_dir = getwd()) {
  if (!requireNamespace("genderdata", quietly = TRUE)) {
    remotes::install_github("lmullen/genderdata")
  }
  # Read the data
  gender_Physicians <- readr::read_csv(input_csv, show_col_types = FALSE)

  # Get first names
  first_names <- gender_Physicians$first_name

  # Genderize the names
  x <- gender::gender(
    names = first_names,
    years = c(1932, 2012),
    method = c("ssa", "ipums", "napp", "kantrowitz", "genderize", "demo"),
    countries = "United States"
  ) %>%
    dplyr::rename(first_name = name) %>%
    dplyr::distinct(first_name, .keep_all = TRUE) %>%
    dplyr::select(-c(year_min, year_max, proportion_female, proportion_male))

  # Rejoin with the original database
  y <- dplyr::left_join(gender_Physicians, x, by = "first_name")

  # Check for missing genders in the joined dataset
  missing_genders_joined <- sum(is.na(y$gender))

  # Generate a timestamp
  timestamp <- format(Sys.time(), "%Y%m%d%H%M%S")

  # Create the output CSV filename with timestamp
  output_csv <- file.path(output_dir, paste0("genderized_", timestamp, "_", basename(input_csv)))

  # Write the result to a CSV file
  readr::write_csv(y, output_csv)

  # Print the number of missing genders in both datasets
  cat("Missing genders in original data:", sum(is.na(x$gender)), "\n")
  cat("Missing genders in joined data:", missing_genders_joined, "\n")

  # Print the path and filename of the new CSV
  cat("Result saved to:", output_csv, "\n")

  # Return the result
  return(y)
}
