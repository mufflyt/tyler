#' Generate overall table
#'
#' Generate an overall table summarizing the demographics of the Table 1.
#'
#' @param input_file_path The path to the data file (in RDS, CSV, or XLS format).
#' @param output_directory The directory where the output table file will be saved.
#'
#' @import arsenal
#' @import readr
#' @import ggplot2
#' @import gridExtra
#' @import tidyverse
#' @import easyr
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Generate the overall table
#' generate_overall_table("data/Table1.rds", "output_tables")
#' }
tm_write2pdf <-
  function(object, filename) {
    print("Function Sanity Check: Creating Arsenal Table as a PDF")
    arsenal::write2pdf(object, here::here("results", (paste0(filename, ".pdf"))),
                       keep.md = TRUE,
                       quiet = TRUE) # passed to rmarkdown::render
  }

# Function to generate overall table and save as PDF
generate_overall_table <- function(input_file_path, output_directory, title = "Overall Table Summary", selected_columns = NULL, label_translations = NULL) {

  cat("Ensure factors have their respective frequency followed. RDS is the preferred file for maintaining the consistency of all data types and factor orderings.")
  # Log function start
  cat("Generating the overall table...\n")

  # Read the data
  data <- readr::read_rds(input_file_path)

  # Check if selected_columns argument is provided
  if (is.null(selected_columns)) {
    # If not provided, use all columns in the data
    cat("Using all columns in the data for the table.\n")
    selected_data <- data
  } else {
    # If selected_columns is provided, select only those columns from the data
    cat("Selecting specific columns for the table.\n")
    selected_data <- data[, selected_columns, drop = FALSE]
  }

  # Log data summary
  cat("Data summary:\n")
  colnames(selected_data)
  str(selected_data)

  # Generate the overall table using arsenal::tableby
  cat("Generating the overall table using arsenal::tableby...\n")
  overall_arsenal_table <- arsenal::tableby(
    ~ .,
    data = selected_data,
    control = tableby.control(
      test = FALSE,
      total = FALSE,
      digits = 0L,
      digits.p = 2L,
      digits.count = 0L,
      numeric.simplify = FALSE,
      cat.simplify = FALSE,
      numeric.stats = c("median", "q1q3"),
      cat.stats = c("countpct"),
      stats.labels = list(
        Nmiss = "N Missing",
        Nmiss2 = "N Missing",
        meansd = "Mean (SD)",
        medianrange = "Median (Range)",
        median = "Median",
        medianq1q3 = "Median (Q1, Q3)",
        q1q3 = "Q1, Q3",
        iqr = "IQR",
        range = "Range",
        countpct = "Count (Pct)",
        Nevents = "Events",
        medSurv = "Median Survival",
        medTime = "Median Follow-Up"
      )
    )
  )

  # Generate the summary of the overall table
  cat("Generating the summary of the overall table...\n")
  overall_summary <- summary(
    overall_arsenal_table,
    text = TRUE,
    labelTranslations = label_translations,
    title = title,
    pfootnote = FALSE
  )

  # Log the overall summary
  cat("Overall table summary:\n")
  print(overall_summary)

  # Access the current date and time
  date_time <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")

  # Create the filename with function name and date-time
  filename <- paste("arsenal_overall_table", date_time, sep = "_")

  # Save the overall table as a PDF
  cat("Saving the overall table as a PDF: ", filename, "\n")
  tm_write2pdf(overall_summary, file.path(output_directory, filename))

  # Log function end
  cat("Overall table generation completed.\n")
}

# # Specify the path to the input RDS file and the output directory for the PDF
# input_file_path <- "/Users/tylermuffly/Dropbox (Personal)/Altschuler/data/ClimbingComplete.rds"
# output_directory <- "tables"
# names(read_rds(input_file_path))
#
#
# # Example 2: Generate table using selected columns from the data
# selected_columns <- c("What is your age?",
#                       "Are you of Hispanic, Latino, or Spanish origin?",
#                       #"How do you identify?",
#                       "Which of the following best describes your race?",
#                       "How many times have you been pregnant (including live births, stillbirths, miscarriages, abortions, and tubal pregnancies)?",
#                       "How many of these pregnancies were miscarriages, abortions, or tubal pregnancies?",
#                       "Have you ever been pregnant?",
#                       "Have you ever delivered a baby?",
#                       "BMI",
#                       "Do you now smoke cigarettes?",
#                       "ACOG_District",
#                       "Are you having sexual relations at this time in your life?"
# )# Replace with your selected column names
#
# # Example label translations
# label_translations <- list(`What is your age?` = "Age, years",
#                            `How many times have you been pregnant (including live births, stillbirths, miscarriages, abortions, and tubal pregnancies)?` = "Gravidity",
#                            `How many of these pregnancies were miscarriages, abortions, or tubal pregnancies?` = "Abortions",
#                            `Have you ever been pregnant?` = "Has been pregnant",
#                            `Have you ever delivered a baby?` = "Parity, delivered a baby",
#                            `Are you of Hispanic, Latino, or Spanish origin?` = "Ethnicity",
#                            `How do you identify?` = "Self-Identifies",
#                            `Which of the following best describes your race?` = "Race",
#                            `BMI` = "Body Mass Index (kg/m^2)",
#                            `Do you now smoke cigarettes?` = "Smoking Status",
#                            `ACOG_District` = "Respondent Location",
#                            `Are you having sexual relations at this time in your life?` = "Sexually active")
#
# generate_overall_table(input_file_path = input_file_path,
#                        output_directory = output_directory,
#                        selected_columns = selected_columns,
#                        title = "Respondent Demographics",
#                        label_translations = label_translations)
