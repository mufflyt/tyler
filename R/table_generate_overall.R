#' Write an Arsenal table to a PDF file
#'
#' Utility wrapper around `arsenal::write2pdf` to save a table object
#' as a PDF document.
#'
#' @param object An `arsenal` table object (or summary thereof) to write.
#' @param filename Path to the output PDF file. A `.pdf` extension is appended
#'   automatically if not already present.
#'
#' @return Invisibly returns the file path.
#'
#' @examples
#' \dontrun{
#' table_write_pdf(overall_summary, "table.pdf")
#' }
#' @export
table_write_pdf <- function(object, filename) {
  if (!requireNamespace("arsenal", quietly = TRUE)) {
    stop("Package 'arsenal' is required for this function. Install with: install.packages('arsenal')", call. = FALSE)
  }
  print("Function Sanity Check: Creating Arsenal Table as a PDF")
  output_file <- if (grepl("\\.pdf$", filename, ignore.case = TRUE)) {
    filename
  } else {
    paste0(filename, ".pdf")
  }

  arsenal::write2pdf(object, output_file, keep.md = TRUE, quiet = TRUE)
  invisible(output_file)
}

#' Generate an overall summary table
#'
#' Generate an overall table summarizing the demographics of the Table 1.
#'
#' @param input_file_path The path to an RDS data file.
#' @param output_directory The directory where the output table file will be saved.
#' @param title The title for the overall table summary (default is "Overall Table Summary").
#' @param selected_columns Optional vector of selected columns to include in the table.
#' @param label_translations Optional named list for label translations.
#' @return Path to the generated PDF file
#'
#' @importFrom readr read_rds
#' @family table
#' @export
#'
#' @examples
#' \dontrun{
#' # Generate the overall table
#' table_generate_overall("data/Table1.rds", "output_tables")
#' }
table_generate_overall <- function(input_file_path, output_directory, title = "Overall Table Summary", selected_columns = NULL, label_translations = NULL) {
  if (!requireNamespace("arsenal", quietly = TRUE)) {
    stop("Package 'arsenal' is required for this function. Install with: install.packages('arsenal')", call. = FALSE)
  }
  message("Ensure factors have their respective frequency followed. RDS is the preferred file for maintaining consistency of data types and factor orderings.")
  # Log function start
  message("Generating the overall table...")

  # Ensure the output directory exists
  if (!dir.exists(output_directory)) {
    message("Creating output directory...")
    dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)
  }

  # Read the data
  message("Reading data from file: ", input_file_path)
  data <- readr::read_rds(input_file_path)

  # Check if the data is empty
  if (nrow(data) == 0 || ncol(data) == 0) {
    stop("The input data is empty.", call. = FALSE)
  }

  # Check if selected_columns argument is provided
  if (is.null(selected_columns)) {
    # If not provided, use all columns in the data
    message("Using all columns in the data for the table.")
    selected_data <- data
  } else {
    # If selected_columns is provided, select only those columns from the data
    message("Selecting specific columns: ", paste(selected_columns, collapse = ", "))
    selected_data <- data[, selected_columns, drop = FALSE]
  }

  # Log data summary
  message("Data summary:")
  print(str(selected_data))

  # Generate the overall table using arsenal::tableby
  message("Generating the overall table using arsenal::tableby...")
  overall_arsenal_table <- arsenal::tableby(
    ~ .,
    data = selected_data,
    control = arsenal::tableby.control(
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
  message("Generating the summary of the overall table...")
  overall_summary <- summary(
    overall_arsenal_table,
    text = TRUE,
    labelTranslations = label_translations,
    title = title,
    pfootnote = FALSE
  )

  # Log the overall summary
  message("Overall table summary:")
  print(overall_summary)

  # Access the current date and time
  date_time <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")

  # Create the filename with function name and date-time
  filename <- file.path(output_directory, paste("arsenal_overall_table", date_time, sep = "_"))

  # Save the overall table as a PDF
  message("Saving the overall table as PDF: ", filename)
  output_path <- table_write_pdf(overall_summary, filename)

  # Log function end
  message("Overall table generation completed.")
  if (requireNamespace("beepr", quietly = TRUE)) {
    beepr::beep(2)
  }

  invisible(output_path)
}
