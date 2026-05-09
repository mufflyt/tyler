# Get Hospital Referral Region Shapefile

This function loads the hospital referral region shapefile and
optionally removes Hawaii and Alaska. The shapefile (~8 MB) is
downloaded from the Dartmouth Atlas website on first use and cached in
the user's R cache directory for subsequent calls.

## Usage

``` r
mysterycall_hrr(remove_HI_AK = TRUE)
```

## Arguments

- remove_HI_AK:

  Logical, should Hawaii and Alaska be removed? Default is TRUE.

## Value

An sf object containing the hospital referral region data.

## See also

[`ensure_hrr_shapefile()`](https://mufflyt.github.io/mysterycall/reference/ensure_hrr_shapefile.md),
[`mysterycall_hrr_maps()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_hrr_maps.html),
[`mysterycall_map_base()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_map_base.html)

Other geospatial helpers:
[`mysterycall_calculate_overlap()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_calculate_overlap.html),
[`mysterycall_geocode()`](https://rdrr.io/pkg/mysterycall/man/mysterycall_geocode.html)

## Examples

``` r
if (FALSE) { # \dontrun{
mysterycall_hrr()
} # }
```
