# Albers Equal-Area CRS for the continental United States

Returns EPSG:5070 (NAD83 / Conus Albers), the standard equal-area
projection used by USGS and the US Census Bureau. Use with
`coord_sf(crs = crs_albers_conus())` to avoid the area distortion of the
default plate carrée projection on national choropleths.

## Usage

``` r
crs_albers_conus()
```

## Value

An [`sf::st_crs`](https://r-spatial.github.io/sf/reference/st_crs.html)
object (EPSG:5070).

## See also

[`theme_green_journal_map()`](https://mufflyt.github.io/mysterycall/reference/theme_green_journal_map.md)

Other green-journal-spatial:
[`compose_map_density()`](https://mufflyt.github.io/mysterycall/reference/compose_map_density.md),
[`truncate_for_viz()`](https://mufflyt.github.io/mysterycall/reference/truncate_for_viz.md),
[`winsorize()`](https://mufflyt.github.io/mysterycall/reference/winsorize.md)

## Examples

``` r
if (FALSE) { # \dontrun{
library(ggplot2)
ggplot(counties_sf) +
  geom_sf(aes(fill = rate)) +
  coord_sf(crs = crs_albers_conus()) +
  theme_green_journal_map()
} # }
```
