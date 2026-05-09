# Internal helper to validate and prepare sf inputs for spatial workflows.

This function ensures objects share a common CRS, have valid geometries,
contain the expected geometry types, possess sensible bounding boxes,
and do not contain empty geometries. Invalid geometries are repaired
with
[`sf::st_make_valid()`](https://r-spatial.github.io/sf/reference/valid.html)
when possible.

## Usage

``` r
validate_sf_inputs(
  ...,
  expected_types = NULL,
  auto_fix = TRUE,
  target_crs = NULL,
  context = "geospatial operation"
)
```

## Arguments

- ...:

  Named sf objects to validate.

- expected_types:

  Named list mapping object names to allowed geometry types. A character
  vector applies to all inputs when names are omitted.

- auto_fix:

  Logical indicating whether invalid geometries should be repaired
  automatically. Defaults to `TRUE`.

- target_crs:

  Optional CRS (numeric EPSG code, proj4string, or
  [`sf::crs`](https://r-spatial.github.io/sf/reference/coerce-methods.html)
  object) that all objects should be transformed into before bounding
  box checks. When `NULL`, the CRS of the first object is used.

- context:

  Character string identifying the calling context for clearer error
  messages.

## Value

A named list of validated sf objects.
