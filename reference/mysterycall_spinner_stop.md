# Stop a spinner

Stop a spinner

## Usage

``` r
mysterycall_spinner_stop(id, result = "done")
```

## Arguments

- id:

  Spinner ID from mysterycall_spinner_start()

- result:

  Result message

## Value

Invisible NULL

## Examples

``` r
# \donttest{
id <- mysterycall_spinner_start("Loading data")
#>   ↻ Loading data...
mysterycall_spinner_stop(id, result = "done")
# }
```
