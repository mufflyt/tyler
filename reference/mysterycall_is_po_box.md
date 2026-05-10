# Detect PO Box Addresses

Checks if an address string represents a Post Office Box. Detects common
variations including "PO BOX", "P O BOX", and "POST OFFICE BOX".

## Usage

``` r
mysterycall_is_po_box(x)

is_po_box(...)
```

## Arguments

- x:

  Character vector of address strings to check.

## Value

Logical vector indicating whether each address is a PO Box.

## See also

Other address-normalization:
[`mysterycall_ascii_norm()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_ascii_norm.md),
[`mysterycall_caps()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_caps.md),
[`mysterycall_has_street_number()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_has_street_number.md),
[`mysterycall_normalize_directionals()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_normalize_directionals.md),
[`mysterycall_normalize_state()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_normalize_state.md),
[`mysterycall_normalize_suffix()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_normalize_suffix.md),
[`mysterycall_normalize_units()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_normalize_units.md),
[`mysterycall_normalize_zip5()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_normalize_zip5.md),
[`mysterycall_strip_suite()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_strip_suite.md),
[`normalize_address_df()`](https://mufflyt.github.io/mysterycall/reference/normalize_address_df.md)

## Examples

``` r
is_po_box("PO Box 12345")
#> Warning: 'is_po_box' is deprecated.
#> Use 'mysterycall_is_po_box' instead.
#> See help("Deprecated")
#> [1] TRUE
is_po_box("123 Main Street")
#> Warning: 'is_po_box' is deprecated.
#> Use 'mysterycall_is_po_box' instead.
#> See help("Deprecated")
#> [1] FALSE
```
