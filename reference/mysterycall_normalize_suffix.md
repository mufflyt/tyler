# Normalize Street Suffixes

Converts full street suffix names (STREET, AVENUE, ROAD, etc.) to their
USPS abbreviations (ST, AVE, RD, etc.).

## Usage

``` r
mysterycall_normalize_suffix(addr)
```

## Arguments

- addr:

  Character vector of address strings to normalize.

## Value

Character vector with street suffixes converted to USPS abbreviations.

## See also

Other address-normalization:
[`mysterycall_ascii_norm()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_ascii_norm.md),
[`mysterycall_caps()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_caps.md),
[`mysterycall_has_street_number()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_has_street_number.md),
[`mysterycall_is_po_box()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_is_po_box.md),
[`mysterycall_normalize_directionals()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_normalize_directionals.md),
[`mysterycall_normalize_state()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_normalize_state.md),
[`mysterycall_normalize_units()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_normalize_units.md),
[`mysterycall_normalize_zip5()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_normalize_zip5.md),
[`mysterycall_strip_suite()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_strip_suite.md),
[`normalize_address_df()`](https://mufflyt.github.io/mysterycall/reference/normalize_address_df.md)

## Examples

``` r
normalize_suffix("123 Main Street")
#> Warning: 'normalize_suffix' is deprecated.
#> Use 'mysterycall_normalize_suffix' instead.
#> See help("Deprecated")
#> [1] "123 MAIN ST"
normalize_suffix("456 Oak Boulevard Suite 100")
#> Warning: 'normalize_suffix' is deprecated.
#> Use 'mysterycall_normalize_suffix' instead.
#> See help("Deprecated")
#> [1] "456 OAK BLVD SUITE 100"
```
