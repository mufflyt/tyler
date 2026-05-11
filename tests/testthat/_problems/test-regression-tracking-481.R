# Extracted from test-regression-tracking.R:481

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "mysterycall", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
library(testthat)
library(mysterycall)
library(dplyr)
source(test_path("../fixtures/baseline_metrics.R"))
create_test_physicians <- function(n = 100) {
  data.frame(
    first = c("Sarah", "Michael", "Jennifer", "Robert", "Lisa",
             "David", "Emily", "James", "Amanda", "Christopher")[rep(1:10, length.out = n)],
    last = c("Johnson", "Chen", "Williams", "Garcia", "Thompson",
            "Martinez", "Davis", "Wilson", "Rodriguez", "Lee")[rep(1:10, length.out = n)],
    practice_name = paste("Practice", 1:n),
    phone_number = sprintf("555-%03d-%04d",
                          sample(100:999, n, replace = TRUE),
                          sample(1000:9999, n, replace = TRUE)),
    state_name = sample(state.name, n, replace = TRUE),
    stringsAsFactors = FALSE
  )
}

# test -------------------------------------------------------------------------
skip_on_cran()
baseline <- BASELINE_METRICS$spatial
results <- data.frame(
    address = paste("Address", 1:100),
    lat = c(
      runif(90, baseline$us_lat_range[1], baseline$us_lat_range[2]),  # US coords
      runif(10, -10, 10)  # Bad coords
    ),
    lon = c(
      runif(90, baseline$us_lon_range[1], baseline$us_lon_range[2]),
      runif(10, -180, -125)
    ),
    stringsAsFactors = FALSE
  )
in_us <- results$lat >= baseline$us_lat_range[1] &
           results$lat <= baseline$us_lat_range[2] &
           results$lon >= baseline$us_lon_range[1] &
           results$lon <= baseline$us_lon_range[2]
us_rate <- sum(in_us) / nrow(results)
expect_gte(us_rate, baseline$us_bounds_rate - 0.05,
            label = sprintf(
              "❌ Too many coordinates outside US: %.1f%% (expected %.1f%%)",
              (1 - us_rate) * 100, (1 - baseline$us_bounds_rate) * 100
            ))
