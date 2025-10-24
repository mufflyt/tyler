local_edition(3)

setup_polygons <- function(crs = 4326) {
  square <- sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0))))
  triangle <- sf::st_polygon(list(rbind(c(0, 0), c(2, 0), c(1, 2), c(0, 0))))

  list(
    bg = sf::st_sf(id = 1, geometry = sf::st_sfc(square, crs = crs)),
    iso = sf::st_sf(id = 1, geometry = sf::st_sfc(triangle, crs = crs))
  )
}

test_that("validate_sf_inputs repairs invalid geometries and enforces expectations", {
  polygons <- setup_polygons()
  invalid_coords <- rbind(c(0, 0), c(1, 1), c(1, 0), c(0, 1), c(0, 0))
  invalid_poly <- sf::st_polygon(list(invalid_coords))
  polygons$bg$geometry <- sf::st_sfc(invalid_poly, crs = 4326)

  validated <- validate_sf_inputs(
    block_groups = polygons$bg,
    isochrones = polygons$iso,
    expected_types = list(
      block_groups = c("POLYGON", "MULTIPOLYGON"),
      isochrones = c("POLYGON", "MULTIPOLYGON")
    ),
    context = "unit test"
  )

  expect_true(all(sf::st_is_valid(validated$block_groups)))
  expect_equal(sf::st_crs(validated$block_groups), sf::st_crs(validated$isochrones))
})

test_that("validate_sf_inputs flags unexpected geometry types", {
  polygons <- setup_polygons()
  point_geom <- sf::st_point(c(5, 5))
  polygons$iso$geometry <- sf::st_sfc(point_geom, crs = 4326)

  expect_error(
    validate_sf_inputs(
      block_groups = polygons$bg,
      isochrones = polygons$iso,
      expected_types = list(
        block_groups = c("POLYGON", "MULTIPOLYGON"),
        isochrones = c("POLYGON", "MULTIPOLYGON")
      ),
      context = "geometry type check"
    ),
    "geometry types"
  )
})

test_that("validate_sf_inputs requires overlapping bounding boxes", {
  polygons <- setup_polygons()
  sf::st_geometry(polygons$iso) <- sf::st_geometry(polygons$iso) + c(100, 100)

  expect_error(
    validate_sf_inputs(
      block_groups = polygons$bg,
      isochrones = polygons$iso,
      expected_types = list(
        block_groups = c("POLYGON", "MULTIPOLYGON"),
        isochrones = c("POLYGON", "MULTIPOLYGON")
      ),
      context = "bounding box check"
    ),
    "Bounding boxes"
  )
})

test_that("validate_sf_inputs catches empty geometries", {
  polygons <- setup_polygons()
  empty_multi <- sf::st_multipolygon()
  polygons$bg$geometry <- sf::st_sfc(empty_multi, crs = 4326)

  expect_error(
    validate_sf_inputs(
      block_groups = polygons$bg,
      isochrones = polygons$iso,
      expected_types = list(
        block_groups = c("POLYGON", "MULTIPOLYGON"),
        isochrones = c("POLYGON", "MULTIPOLYGON")
      ),
      context = "empty geometry"
    ),
    "empty geometries"
  )
})
