#' Install and Load Required Libraries
#'
#' This function installs and loads the necessary libraries for your analysis.
#' It checks if the packages are already installed and installs them if needed.
#' It then loads the installed packages.
#'
#' @return A character vector containing the names of the loaded packages.
#'
#' @importFrom remotes install_github
#' @importFrom exploratory
#' @importFrom devtools install_github
#' @importFrom renv snapshot
#' @importFrom tidyverse install.packages
#'
#' @examples
#' install_and_load_libraries()
#'
#' @export

install_and_load_libraries <- function() {
  library(devtools)
  library(rlang)

  # List of packages to install and load
  cran_pkgs <- c(
    "AppliedPredictiveModeling", "arsenal", "BH", "Boruta", "caTools", "car",
    "caret", "caretEnsemble", "compareGroups", "corrgram", "corrplot", "cowplot",
    "data.table", "devtools", "discrim", "doMC", "doParallel", "DT", "dtplyr",
    "DynNom", "epiDisplay", "ezknitr", "ezplot", "factoextra", "fastDummies",
    "fansi", "flexdashboard", "foreach", "fs", "gbm", "ggforce", "ggplot2",
    "ggthemes", "glmnet", "gmlang/ezkable", "gss", "here", "highr", "Hmisc",
    "ipred", "Janitor", "kableExtra", "klaR", "knitr", "kntd", "labeling",
    "lime", "lmtest", "magick", "mctest", "Metrics", "mice", "MLmetrics",
    "ModelMetrics", "moments", "mosaic", "MLmetrics", "mice", "mosaic", "mltools",
    "naivebayes", "naniar", "odbc", "openxlsx", "pander", "pdp", "PerformanceAnalytics",
    "perturbR", "plotly", "plotROC", "pROC", "pscl", "psych", "quanteda", "R.methodsS3",
    "R2HTML", "ranger", "rattle", "RANN", "rcatboost", "RCurl", "rmarkdown",
    "ROCR", "rsconnect", "rvest", "rwekajars", "RSQLite", "RTextTools", "R.utils",
    "sandwich", "scales", "skimr", "sjPlot", "sjmisc", "slam", "stringr", "styler",
    "summarytools", "tableone", "tibble", "tidylog", "tidyquant", "tidyr", "tidyverse",
    "timeDate", "tm", "utf8", "vip", "viridis", "visNetwork", "wordcloud"
  )

  # List of GitHub packages to install and load
  githb_pkgs <- c(
    "gmlang/ezkable", "gmlang/ezplot"
  )

  # List of remotes packages to install and load
  remotes_pkg <- c(
    "lmullen/genderdata"
  )

  # List of exploratory packages to install and load
  exploratory_pkg <- c(
    "exploratory-io/exploratory_func", "paulhendricks/anonymizer", "r-lib/callr",
    "r-link/corrmorant", "tidyverse/glue"
  )

  # Function to install and load packages
  install_load_pkg <- function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      cat(pkg, "missing, installing...\n")
      if (pkg %in% cran_pkgs) {
        devtools::install.packages(pkg, dependencies = TRUE)
      } else if (pkg %in% githb_pkgs) {
        devtools::install_github(paste0("gmlang/", pkg))
      } else if (pkg %in% remotes_pkg) {
        remotes::install_github(pkg, dependencies = c("Depends"))
      } else if (pkg %in% exploratory_pkg) {
        devtools::install_github(pkg, force = FALSE, dependencies = c("Depends"), upgrade = c("never"))
      }
    }
    library(pkg, character.only = TRUE)
    pkg
  }

  # Install and load packages
  loaded_pkgs <- c(
    sapply(cran_pkgs, install_load_pkg),
    sapply(githb_pkgs, install_load_pkg),
    sapply(remotes_pkg, install_load_pkg),
    sapply(exploratory_pkg, install_load_pkg)
  )

  # Load the exploratory package
  library("exploratory")

  # Save the current R environment as a snapshot
  renv::snapshot()

  return(loaded_pkgs)
}

# Call the function to install and load libraries
# install_and_load_libraries()
