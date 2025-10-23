#' Generate overall table
#'
#' Generate an overall table summarizing the demographics of the Table 1.
#'
#' @param input_file_path The path to the data file (in RDS, CSV, or XLS format).
#' @param output_directory The directory where the output table file will be saved.
#' @param title The title for the overall table summary (default is "Overall Table Summary").
#' @param selected_columns Optional vector of selected columns to include in the table.
#' @param label_translations Optional named list for label translations.
#' @param verbose Logical; if TRUE, prints status messages while running. Default is FALSE.
#' @return Path to the generated PDF file
#'
#' @importFrom arsenal write2pdf tableby
#' @importFrom readr read_rds
#' @importFrom easyr read.any
#' @importFrom fs dir_create dir_exists
#' @importFrom here here
#' @family table
#' @export
#'
#' @examples
#' \dontrun{
#' # Generate the overall table
#' generate_overall_table("data/Table1.rds", "output_tables")
#' }

#' Write an Arsenal table to a PDF file
#'
#' Utility wrapper around `arsenal::write2pdf` to save a table object
#' as a PDF document.
#'
#' @param object An `arsenal` table object to write.
#' @param filename Path to the output PDF file.
#' @param verbose Logical; if TRUE, prints status messages while running. Default is FALSE.
#'
#' @return Invisibly returns the file path.
#'
#' @examples
#' \dontrun{
#' tm_write2pdf(overall_summary, "table.pdf")
#' }
#' @export
tm_write2pdf <- function(object, filename, verbose = FALSE) {
  if (isTRUE(verbose)) {
    message("Function Sanity Check: Creating Arsenal Table as a PDF")
  }
  arsenal::write2pdf(object, filename, keep.md = TRUE, quiet = TRUE)
}

generate_overall_table <- function(input_file_path, output_directory, title = "Overall Table Summary", selected_columns = NULL, label_translations = NULL, verbose = FALSE) {
  if (isTRUE(verbose)) {
    message("Ensure factors have their respective frequency followed. RDS is the preferred file for maintaining the consistency of all data types and factor orderings.")
    message("Generating the overall table...")
  }

  # Ensure the output directory exists
  if (!fs::dir_exists(output_directory)) {
    if (isTRUE(verbose)) {
      message("Creating output directory...")
    }
    fs::dir_create(output_directory)
  }

  # Read the data
  if (isTRUE(verbose)) {
    message("Reading data from file: ", input_file_path)
  }
  data <- readr::read_rds(input_file_path)

  # Check if the data is empty
  if (nrow(data) == 0 || ncol(data) == 0) {
    stop("The input data is empty.")
  }

  # Check if selected_columns argument is provided
  if (is.null(selected_columns)) {
    # If not provided, use all columns in the data
    if (isTRUE(verbose)) {
      message("Using all columns in the data for the table.")
    }
    selected_data <- data
  } else {
    # If selected_columns is provided, select only those columns from the data
    if (isTRUE(verbose)) {
      message("Selecting specific columns for the table: ", paste(selected_columns, collapse = ", "))
    }
    selected_data <- data[, selected_columns, drop = FALSE]
  }

  # Log data summary
  if (isTRUE(verbose)) {
    message("Data summary:")
    print(str(selected_data))
  }

  # Generate the overall table using arsenal::tableby
  if (isTRUE(verbose)) {
    message("Generating the overall table using arsenal::tableby...")
  }
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
  if (isTRUE(verbose)) {
    message("Generating the summary of the overall table...")
  }
  overall_summary <- summary(
    overall_arsenal_table,
    text = TRUE,
    labelTranslations = label_translations,
    title = title,
    pfootnote = FALSE
  )

  # Log the overall summary
  if (isTRUE(verbose)) {
    message("Overall table summary:")
    print(overall_summary)
  }

  # Access the current date and time
  date_time <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")

  # Create the filename with function name and date-time
  filename <- file.path(output_directory, paste("arsenal_overall_table", date_time, sep = "_"))

  # Save the overall table as a PDF
  if (isTRUE(verbose)) {
    message("Saving the overall table as a PDF: ", filename)
  }
  tm_write2pdf(overall_summary, filename, verbose = verbose)

  # Log function end
  if (isTRUE(verbose)) {
    message("Overall table generation completed.")
  }
  beepr::beep(2)
}
