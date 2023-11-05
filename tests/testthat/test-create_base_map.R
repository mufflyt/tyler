library(testthat)
library(tyler)  # Replace with your package name
library(leaflet)  # Load the leaflet package

context("create_base_map")

# Test that create_base_map returns a Leaflet map object
test_that("create_base_map returns a Leaflet map object", {
  my_map <- create_base_map("<h1>Custom Map Title</h1>")
  expect_s3_class(my_map, "leaflet")
})

# Add more tests as needed for specific functionality of create_base_map
