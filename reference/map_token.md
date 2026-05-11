# Map Token Replacements Using Word Boundaries

Performs word-level replacements using a named character vector as a
lookup map. Replacements are made at word boundaries to avoid partial
matches.

## Usage

``` r
map_token(x, map)
```

## Arguments

- x:

  Character vector of strings to process.

- map:

  Named character vector where names are patterns to match and values
  are replacement strings.

## Value

Character vector with all matching tokens replaced.

## Examples

``` r
if (FALSE) {
dir_map <- c("NORTH" = "N", "SOUTH" = "S")
map_token("123 NORTH MAIN STREET", dir_map)
}
```
