#' Writes an Arsenal table object to a Word document.
#'
#' @param object An object to be written to Word, typically an Arsenal table.
#' @param filename The filename (without extension) for the Word document.
#' @return None
#' @export
#'
#' @importFrom arsenal write2word
#'
#' @examples
#' \dontrun{
#' arsenal_tables_write2word(my_table, "output_table")
#' }
arsenal_tables_write2word <- function(object, filename) {
  # Validate input parameters
  if (missing(object)) {
    stop("Error: 'object' is required.")
  }
  if (!is.character(filename)) {
    stop("Error: 'filename' must be a character string.")
  }

  # Check if the "tables" directory exists; if not, create it
  if (!dir.exists("tables")) {
    dir.create("tables")
  }

  # Create full path for the Word file
  word_path <- file.path("tables", paste0(filename, ".docx"))

  message("Creating Arsenal table as a Word document...")
  tryCatch({
    arsenal::write2word(
      object,
      file = word_path,
      keep.md = FALSE,
      quiet = TRUE
    )
  }, error = function(e) {
    stop("Error occurred while writing to Word document:", e$message)
  })

  # Print the full path to the saved file
  cat("Word file saved successfully at", word_path, "\n")
}
