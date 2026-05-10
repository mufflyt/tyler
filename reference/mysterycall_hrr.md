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
[`mysterycall_hrr_maps()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_hrr_maps.md),
[`mysterycall_map_base()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_map_base.md)

Other geospatial helpers:
[`mysterycall_calculate_overlap()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_calculate_overlap.md),
[`mysterycall_geocode()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_geocode.md)

## Examples

``` r
if (FALSE) { # interactive()
mysterycall_hrr()
}
```
