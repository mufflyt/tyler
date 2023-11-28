#' Generate overall table
#'
#' Generate an overall table summarizing the demographics of the Table 1.
#'
#' @param input_file The path to the data file (in RDS, CSV, or XLS format).
#' @param output_dir The directory where the output table file will be saved.
#'
#' @import arsenal
#' @importFrom readr
#' @import ggplot2
#' @import gridExtra
#' @import tidyverse
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
    #pass filename and title with quotations, do not add .pdf
    print("Function Sanity Check: Creating Arsenal Table as a PDF")
    arsenal::write2pdf(object, (here::here("results", (paste0(filename, ".pdf")))),
                       #puts the file into a subdirectory called results
                       keep.md = TRUE,
                       quiet = TRUE) # passed to rmarkdown::render
  }

generate_overall_table <- function(input_file, output_dir) {
  # Log function start
  cat("Generating the overall table...\n")

  # Read data from the input file
  cat("Reading data from input file: ", input_file, "\n")

  # Check the file extension and read the data accordingly
  if (tolower(tools::file_ext(input_file)) == "rds") {
    x <- readr::read_rds(input_file)
  } else if (tolower(tools::file_ext(input_file)) == "csv") {
    x <- readr::read_csv(input_file)
  } else if (tolower(tools::file_ext(input_file)) %in% c("xls", "xlsx")) {
    x <- readxl::read_xls(input_file)
  } else {
    stop("Unsupported file format. Please provide an RDS, CSV, or XLS file.")
  }

  # Log data summary
  cat("Data summary:\n")
  colnames(x)
  str(x)

  # Generate the overall table using arsenal::tableby
  cat("Generating the overall table using arsenal::tableby...\n")
  overall_arsenal_table <- arsenal::tableby(
    ~.,
    data = x,
    control = tableby.control(
      test = F,
      total = F,
      digits = 0L,
      digits.p = 2L,
      digits.count = 0L,
      numeric.simplify = FALSE,
      cat.simplify = FALSE,
      numeric.stats = c("median", "q1q3"),
      cat.stats = c("Nmiss", "countpct"),
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
  overall <- summary(
    overall_arsenal_table,
    text = TRUE,
    title = "Table: Characteristics of Obstetrics and Gynecology Subspecialists Practicing at Obstetrics and Gynecology Residency Programs",
    pfootnote = FALSE
  )

  # Log the overall summary
  cat("Overall table summary:\n")
  print(overall)

  # Access the current date and time
  date_time <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")

  # Create the filename with function name and date-time
  filename <-
    paste("arsenal_overall_table_one", date_time, ".pdf", sep = "_")

  # Save the overall table as a PDF
  cat("Saving the overall table as a PDF: ", filename, "\n")
  tm_write2pdf(overall, file.path(output_dir, filename))

  # Log function end
  cat("Overall table generation completed.\n")
}
