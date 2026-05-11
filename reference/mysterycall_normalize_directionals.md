# Normalize Directional Prefixes and Suffixes

Converts full directional words (NORTH, SOUTH, EAST, WEST) and compound
directions (NORTHEAST, etc.) to USPS abbreviations (N, S, E, W, NE,
etc.).

## Usage

``` r
mysterycall_normalize_directionals(addr)
```

## Arguments

- addr:

  Character vector of address strings to normalize.

## Value

Character vector with directionals converted to USPS abbreviations.

## See also

Other address-normalization:
[`mysterycall_ascii_norm()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_ascii_norm.md),
[`mysterycall_caps()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_caps.md),
[`mysterycall_has_street_number()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_has_street_number.md),
[`mysterycall_is_po_box()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_is_po_box.md),
[`mysterycall_normalize_state()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_normalize_state.md),
[`mysterycall_normalize_suffix()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_normalize_suffix.md),
[`mysterycall_normalize_units()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_normalize_units.md),
[`mysterycall_normalize_zip5()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_normalize_zip5.md),
[`mysterycall_strip_suite()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_strip_suite.md),
[`normalize_address_df()`](https://mufflyt.github.io/mysterycall/reference/normalize_address_df.md)

## Examples

``` r
normalize_directionals("123 North Main Street")
#> Warning: 'normalize_directionals' is deprecated.
#> Use 'mysterycall_normalize_directionals' instead.
#> See help("Deprecated")
#> [1] "123 N MAIN STREET"
normalize_directionals("456 Southeast Oak Avenue")
#> Warning: 'normalize_directionals' is deprecated.
#> Use 'mysterycall_normalize_directionals' instead.
#> See help("Deprecated")
#> [1] "456 SE OAK AVENUE"
```
