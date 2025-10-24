#' Writes an Arsenal table object to a Word document.
#'
#' @param object An object to be written to Word, typically an Arsenal table.
#' @param filename The filename (without extension) for the Word document.
#' @param output_dir Directory where the Word document should be written.
#'   Defaults to a session-specific folder inside [tempdir()].
#' @return None
#' @export
#'
#' @importFrom arsenal write2word
#'
#' @examples
#' \dontrun{
#' arsenal_tables_write2word(my_table, "output_table")
#' }
arsenal_tables_write2word <- function(object, filename, output_dir = NULL) {
  # Validate input parameters
  if (!is.data.frame(object)) {
    stop("Error: 'object' must be a data frame object.")
  }
  if (!is.character(filename)) {
    stop("Error: 'filename' must be a character string.")
  }

  if (is.null(output_dir)) {
    output_dir <- tyler_tempdir("tables", create = TRUE)
  } else if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  # Create full path for the Word file
  word_path <- file.path(output_dir, paste0(filename, ".docx"))

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
