# Generate Hexagon Maps for Hospital Referral Regions (HRR)

This function generates hexagon maps for hospital referral regions.

## Usage

``` r
mysterycall_hrr_maps(
  physician_sf,
  trait_map = "all",
  honey_map = "all",
  output_dir = NULL,
  dpi = 600,
  width = 7,
  height = 5
)
```

## Arguments

- physician_sf:

  An sf object containing physician data with coordinates.

- trait_map:

  A string specifying the trait map (default is "all").

- honey_map:

  A string specifying the honey map (default is "all").

- output_dir:

  Directory where generated figures are written. Defaults to a
  session-specific folder inside
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html).

- dpi:

  Resolution used when saving the final figure (default is 600).

- width:

  Final figure width in inches for journal submission (default is 7).

- height:

  Final figure height in inches for journal submission (default is 5).

## Value

Invisibly returns the arranged grob object containing the contiguous US
map and Alaska/Hawaii/Puerto Rico inset maps.

## See also

[`mysterycall_hrr()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_hrr.html),
[`mysterycall_map_base()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_map_base.html),
[`mysterycall_map_block_group()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_map_block_group.html)

## Examples

``` r
if (FALSE) { # \dontrun{
mysterycall_hrr_maps(physician_sf)
} # }
```
