library(sf)

test_that("map_create_block_group_overlap requires drive_time column", {
  square <- sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0))))
  bg <- sf::st_sf(
    GEOID = "000000000000",
    overlap = 0.5,
    vintage = 2020,
    geometry = sf::st_sfc(square, crs = 4326)
  )
  iso <- sf::st_sf(
    name = "iso",
    geometry = sf::st_sfc(square, crs = 4326)
  )

  expect_error(
    map_create_block_group_overlap(bg, iso, output_dir = tempdir()),
    "drive_time"
  )
})
