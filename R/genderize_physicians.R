#' Genderize Physicians Data
#'
#' This function reads a CSV file containing physician data, genderizes the first names,
#' and joins the gender information back to the original data. It then saves the
#' result to a new CSV file with a timestamp.
#'
#' @param input_csv The path to the input CSV file containing physician data.
#' @return A data frame with genderized information joined to the original data.
#'
#' @import gender
#' @import dplyr
#' @import readr
#'
#' @examples
#' \dontrun{
#' result <- genderize_physicians("sample.csv")
#' }
#'
#' @export
genderize_physicians <- function(input_csv) {
  # Read the data
  gender_Physicians <- readr::read_csv(input_csv)

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
  output_csv <- paste0("genderized_", timestamp, "_", basename(input_csv))

  # Get the full path and filename of the output CSV
  output_path <- file.path(getwd(), output_csv)

  # Write the result to a CSV file
  readr::write_csv(y, output_path)

  # Print the number of missing genders in both datasets
  cat("Missing genders in original data:", sum(is.na(x$gender)), "\n")
  cat("Missing genders in joined data:", missing_genders_joined, "\n")

  # Print the path and filename of the new CSV
  cat("Result saved to:", output_path, "\n")

  # Return the result
  #return(y)
}

# Usage example:
# result <- genderize_physicians("sample.csv")

#readr::read_csv(("/Users/tylermuffly/Dropbox (Personal)/Indian_dev/Akapo_Muffly_Coelho/data/gender_Physicians_2021_join_24.csv")) %>% head(10)
