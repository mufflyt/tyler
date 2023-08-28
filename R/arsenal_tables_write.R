#' Save Arsenal Table as PDF
#'
#' This function takes an Arsenal Table object and saves it as a PDF file in the "tables" directory.
#'
#' @param object An Arsenal Table object.
#' @param filename A string representing the desired filename without extension.
#' @examples
#' \dontrun{
#' overall <- summary(
#'   overall_arsenal_table,
#'   text = T,
#'   title = "Table: Characteristics of Obstetrics and Gynecology Subspecialists Practicing at Obstetrics and Gynecology Residency Programs",
#'   pfootnote = FALSE
#' )
#' arsenal_tables_write2pdf(overall, "arsenal_overall_table_one")
#' }
#' @export
arsenal_tables_write2pdf <- function(object, filename) {
  # Check if the "tables" directory exists; if not, create it
  if (!dir.exists("tables")) {
    dir.create("tables")
  }

  # Create full path for the PDF file
  pdf_path <- paste0("tables/", filename, ".pdf")

  print("Function Sanity Check: Creating Arsenal Table as a PDF")
  arsenal::write2pdf(object,
    file = pdf_path,
    keep.md = FALSE,
    quiet = TRUE
  ) # passed to rmarkdown::render

  # Print the full path to the saved file
  cat("PDF file saved successfully at", pdf_path, "\n")
}

#' Save Arsenal Table as Word Document
#'
#' This function takes an Arsenal Table object and saves it as a Word document in the "tables" directory.
#'
#' @param object An Arsenal Table object.
#' @param filename A string representing the desired filename without extension.
#' @examples
#' \dontrun{
#' overall <- summary(
#'   overall_arsenal_table,
#'   text = T,
#'   title = "Table: Characteristics of Obstetrics and Gynecology Subspecialists Practicing at Obstetrics and Gynecology Residency Programs",
#'   pfootnote = FALSE
#' )
#' arsenal_tables_write2word(overall, "arsenal_overall_table_one")
#' }
#' @export
arsenal_tables_write2word <- function(object, filename) {
  # Check if the "tables" directory exists; if not, create it
  if (!dir.exists("tables")) {
    dir.create("tables")
  }

  # Create full path for the Word file
  word_path <- paste0("tables/", filename, ".docx")

  print("Function Sanity Check: Creating Arsenal Table as a Word Document")
  arsenal::write2word(object,
    file = word_path,
    keep.md = FALSE,
    quiet = TRUE
  ) # passed to rmarkdown::render

  # Print the full path to the saved file
  cat("Word file saved successfully at", word_path, "\n")
}
