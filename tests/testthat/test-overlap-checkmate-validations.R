library(sf)

make_square_sf <- function(...) {
  square <- sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0))))
  sf::st_sf(..., geometry = sf::st_sfc(square, crs = 4326))
}

test_that("calculate_intersection_overlap_and_save checkmate validations trigger", {
  bg <- make_square_sf(GEOID = "000000000000", vintage = 2020)
  iso <- make_square_sf(drive_time = 30, data_year = 2020)

  expect_error(calculate_intersection_overlap_and_save(NULL, iso, 30, tempdir(), notify = FALSE), "block_groups")
  expect_error(calculate_intersection_overlap_and_save(bg, NULL, 30, tempdir(), notify = FALSE), "isochrones_joined")
  expect_error(calculate_intersection_overlap_and_save(bg, iso, -1, tempdir(), notify = FALSE), "drive_time_minutes")
  expect_error(calculate_intersection_overlap_and_save(bg, iso, 30, "", notify = FALSE), "output_dir")
  expect_error(calculate_intersection_overlap_and_save(bg, iso, 30, tempdir(), notify = "no"), "notify")
})

test_that("map_create_block_group_overlap checkmate validations trigger", {
  bg <- make_square_sf(GEOID = "000000000000", NAMELSAD = "BG", overlap = 0.5, vintage = 2020)
  iso <- make_square_sf(drive_time = 30)

  expect_error(map_create_block_group_overlap(NULL, iso, tempdir()), "bg_data")
  expect_error(map_create_block_group_overlap(bg, NULL, tempdir()), "isochrones_data")
  expect_error(map_create_block_group_overlap(bg, iso, ""), "output_dir")
  expect_error(map_create_block_group_overlap(bg |> dplyr::mutate(overlap = 1.5), iso, tempdir()), "bg_data\\$overlap")
  expect_error(map_create_block_group_overlap(bg, iso |> dplyr::mutate(drive_time = "thirty"), tempdir()), "drive_time")
})
