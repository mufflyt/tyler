# Create a Configurable Leaflet Base Map

Build a Leaflet base map with sensible defaults for the mysterycall
mapping helpers. The map includes multiple tile providers, a scale bar,
optional title control, and centers on the continental United States by
default.

## Usage

``` r
mysterycall_map_base(title = NULL, lat = 39.8282, lng = -98.5795, zoom = 4)
```

## Arguments

- title:

  Optional HTML string used for a title control in the upper left corner
  of the map. Supply `NULL` or an empty string to omit the control.

- lat, lng:

  Numeric latitude and longitude used to center the initial view.
  Defaults position the map over the continental United States.

- zoom:

  Numeric zoom level passed to
  [`leaflet::setView()`](https://rstudio.github.io/leaflet/reference/map-methods.html).

## Value

A
[`leaflet::leaflet()`](https://rstudio.github.io/leaflet/reference/leaflet.html)
map object pre-configured with controls and basemap layers.

## See also

Other mapping:
[`mysterycall_clear_isochrone_cache()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_clear_isochrone_cache.md),
[`mysterycall_create_isochrones()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_create_isochrones.md),
[`mysterycall_isochrones_for_df()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_isochrones_for_df.md),
[`mysterycall_map_acceptance_rate()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_map_acceptance_rate.md),
[`mysterycall_map_acog_districts()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_map_acog_districts.md),
[`mysterycall_map_block_group()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_map_block_group.md),
[`mysterycall_map_leaflet()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_map_leaflet.md),
[`mysterycall_map_physicians()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_map_physicians.md),
[`mysterycall_plot_density()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_plot_density.md),
[`mysterycall_plot_isochrones()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_plot_isochrones.md),
[`mysterycall_plot_line()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_plot_line.md),
[`mysterycall_plot_scatter()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_plot_scatter.md)

## Examples

``` r
# \donttest{
mysterycall_map_base()

{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG3857","code":null,"proj4def":null,"projectedBounds":null,"options":{}},"zoomControl":true},"calls":[{"method":"addProviderTiles","args":["CartoDB.Voyager",null,"CartoDB Voyager",{"errorTileUrl":"","noWrap":false,"detectRetina":false}]},{"method":"addProviderTiles","args":["Stadia.StamenTonerLite",null,"Toner Lite",{"errorTileUrl":"","noWrap":false,"detectRetina":false}]},{"method":"addScaleBar","args":[{"maxWidth":100,"metric":true,"imperial":true,"updateWhenIdle":true,"position":"bottomleft"}]},{"method":"addLayersControl","args":[["CartoDB Voyager","Toner Lite"],[],{"collapsed":false,"autoZIndex":true,"position":"topright"}]},{"method":"addTiles","args":["https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",null,null,{"minZoom":0,"maxZoom":18,"tileSize":256,"subdomains":"abc","errorTileUrl":"","tms":false,"noWrap":false,"zoomOffset":0,"zoomReverse":false,"opacity":1,"zIndex":1,"detectRetina":false,"useCache":true,"crossOrigin":true,"attribution":"&copy; <a href=\"https://openstreetmap.org/copyright/\">OpenStreetMap<\/a>,  <a href=\"https://opendatacommons.org/licenses/odbl/\">ODbL<\/a>"}]}],"setView":[[39.8282,-98.5795],4,[]]},"evals":[],"jsHooks":[]}mysterycall_map_base("<strong>Custom title</strong>")

{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG3857","code":null,"proj4def":null,"projectedBounds":null,"options":{}},"zoomControl":true},"calls":[{"method":"addProviderTiles","args":["CartoDB.Voyager",null,"CartoDB Voyager",{"errorTileUrl":"","noWrap":false,"detectRetina":false}]},{"method":"addProviderTiles","args":["Stadia.StamenTonerLite",null,"Toner Lite",{"errorTileUrl":"","noWrap":false,"detectRetina":false}]},{"method":"addScaleBar","args":[{"maxWidth":100,"metric":true,"imperial":true,"updateWhenIdle":true,"position":"bottomleft"}]},{"method":"addLayersControl","args":[["CartoDB Voyager","Toner Lite"],[],{"collapsed":false,"autoZIndex":true,"position":"topright"}]},{"method":"addTiles","args":["https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",null,null,{"minZoom":0,"maxZoom":18,"tileSize":256,"subdomains":"abc","errorTileUrl":"","tms":false,"noWrap":false,"zoomOffset":0,"zoomReverse":false,"opacity":1,"zIndex":1,"detectRetina":false,"useCache":true,"crossOrigin":true,"attribution":"&copy; <a href=\"https://openstreetmap.org/copyright/\">OpenStreetMap<\/a>,  <a href=\"https://opendatacommons.org/licenses/odbl/\">ODbL<\/a>"}]},{"method":"addControl","args":["<div class=\"mysterycall-map-title\"><strong>Custom title<\/strong><\/div>","topleft",null,"info legend"]}],"setView":[[39.8282,-98.5795],4,[]]},"evals":[],"jsHooks":[]}# }
```
