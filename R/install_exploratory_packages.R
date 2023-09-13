#' Install Exploratory Packages
#'
#' This function checks and installs necessary packages for Exploratory Data Analysis in R.
#'
#' @description
#' This function checks if required packages for Exploratory Data Analysis are installed and installs them if necessary.
#' It includes packages from both GitHub and CRAN repositories.
#'
#' @details
#' This function checks and installs the following packages:
#'   - From GitHub: anonymizer, glue, exploratory_func.
#'   - From CRAN: backports, psych, qlcMatrix, quanteda, textshape, mnormt, sparsesvd, docopt, proxyC.
#'
#' @seealso
#' \code{\link{install.packages}} to install packages from CRAN.
#' \code{\link{install_github}} to install packages from GitHub.
#'
#' @return None
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Call the function to install and load packages
#' install_exploratory_packages()
#' }
install_exploratory_packages <- function() {
  # Check and install devtools if necessary
  if (packageVersion("devtools") < 1.6) {
    install.packages("devtools")
  }

  # Install packages from GitHub
  devtools_packages <- c("paulhendricks/anonymizer", "tidyverse/glue", "exploratory-io/exploratory_func")
  for (pkg in devtools_packages) {
    devtools::install_github(pkg)
  }

  # Install packages from CRAN
  cran_packages <- c("backports", "psych", "qlcMatrix", "quanteda", "textshape", "mnormt", "sparsesvd", "docopt", "proxyC")
  new_packages <- cran_packages[!(cran_packages %in% installed.packages()[,"Package"])]
  if (length(new_packages)) install.packages(new_packages)

  # Load libraries
  libraries <- c("devtools", "anonymizer", "glue", "backports", "psych", "qlcMatrix", "quanteda", "textshape", "mnormt", "sparsesvd", "docopt", "proxyC", "exploratory")
  for (lib in libraries) {
    library(lib, character.only = TRUE)
  }
}
