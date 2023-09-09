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

# Call the function to install and load packages
#install_exploratory_packages()
