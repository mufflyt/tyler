#' ACOG Districts Data
#'
#' This dataset contains information about American College of Obstetricians and Gynecologists (ACOG) districts, including their two-letter state abbreviations and full state names.
#'
#' @return A tibble where each row represents an ACOG district with its
#'   corresponding two-letter abbreviation and full state name.
#'
#' @format A data frame with the following columns:
#' \describe{
#'   \item{ACOG_District}{Two-letter state abbreviations representing ACOG districts.}
#'   \item{name}{Full names of the states corresponding to the ACOG districts.}
#' }
#'
#' @source Data was obtained from the official ACOG website: <https://www.acog.org/community/districts-and-sections>
#'
#' @examples
#' # Load the ACOG Districts Data
#' data(ACOG_Districts)
#'
#' # Inspect the dataset
#' print(ACOG_Districts)
#'
#' # Get a summary of the dataset
#' summary(ACOG_Districts)
#'
#' # Perform data analysis and exploration
#'
#' @keywords dataset
#' @family datasets
"ACOG_Districts"
