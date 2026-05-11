# Detect Addresses with Street Numbers

Checks if an address string begins with a numeric street number.

## Usage

``` r
mysterycall_has_street_number(x)
```

## Arguments

- x:

  Character vector of address strings to check.

## Value

Logical vector indicating whether each address starts with a number.

## See also

Other address-normalization:
[`mysterycall_ascii_norm()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_ascii_norm.md),
[`mysterycall_caps()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_caps.md),
[`mysterycall_is_po_box()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_is_po_box.md),
[`mysterycall_normalize_directionals()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_normalize_directionals.md),
[`mysterycall_normalize_state()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_normalize_state.md),
[`mysterycall_normalize_suffix()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_normalize_suffix.md),
[`mysterycall_normalize_units()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_normalize_units.md),
[`mysterycall_normalize_zip5()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_normalize_zip5.md),
[`mysterycall_strip_suite()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_strip_suite.md),
[`normalize_address_df()`](https://mufflyt.github.io/mysterycall/reference/normalize_address_df.md)

## Examples

``` r
has_street_number("123 Main Street")
#> Warning: 'has_street_number' is deprecated.
#> Use 'mysterycall_has_street_number' instead.
#> See help("Deprecated")
#> [1] TRUE
has_street_number("University Medical Center")
#> Warning: 'has_street_number' is deprecated.
#> Use 'mysterycall_has_street_number' instead.
#> See help("Deprecated")
#> [1] FALSE
```
