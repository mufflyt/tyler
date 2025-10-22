# Test Helper: Skip tests if packages not available
# Place in: tests/testthat/helper-skip-if-not-installed.R

skip_if_not_installed <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    testthat::skip(paste0("Package '", pkg, "' not available"))
  }
}

skip_if_no_spatial <- function() {
  pkgs <- c("sf", "tmap", "leaflet", "tigris")
  missing <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
  if (length(missing) > 0) {
    testthat::skip(paste0("Spatial packages not available: ", paste(missing, collapse = ", ")))
  }
}

skip_if_no_genderdata <- function() {
  if (!requireNamespace("genderdata", quietly = TRUE)) {
    testthat::skip("Package 'genderdata' not available (GitHub only)")
  }
}

skip_on_ci_if_slow <- function() {
  if (identical(Sys.getenv("CI"), "true")) {
    testthat::skip("Skipping slow test on CI")
  }
}

# Skip if package is in Suggests but not installed
skip_if_suggested_not_installed <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    testthat::skip(paste0("Suggested package '", pkg, "' not installed"))
  }
}
