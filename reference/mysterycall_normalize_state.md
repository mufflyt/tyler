# Normalize State Names to USPS Codes

Converts full state names to their two-letter USPS codes. If the input
is already a valid two-letter code, it is returned unchanged.

## Usage

``` r
mysterycall_normalize_state(state)
```

## Arguments

- state:

  Character vector of state names or abbreviations.

## Value

Character vector of two-letter USPS state codes.

## See also

Other address-normalization:
[`mysterycall_ascii_norm()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_ascii_norm.md),
[`mysterycall_caps()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_caps.md),
[`mysterycall_has_street_number()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_has_street_number.md),
[`mysterycall_is_po_box()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_is_po_box.md),
[`mysterycall_normalize_directionals()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_normalize_directionals.md),
[`mysterycall_normalize_suffix()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_normalize_suffix.md),
[`mysterycall_normalize_units()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_normalize_units.md),
[`mysterycall_normalize_zip5()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_normalize_zip5.md),
[`mysterycall_strip_suite()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_strip_suite.md),
[`normalize_address_df()`](https://mufflyt.github.io/mysterycall/reference/normalize_address_df.md)

## Examples

``` r
normalize_state("California")
#> Warning: 'normalize_state' is deprecated.
#> Use 'mysterycall_normalize_state' instead.
#> See help("Deprecated")
#> [1] "CA"
normalize_state("NY")
#> Warning: 'normalize_state' is deprecated.
#> Use 'mysterycall_normalize_state' instead.
#> See help("Deprecated")
#> [1] "NY"
```
