# Create a simple progress callback for batch operations

Create a simple progress callback for batch operations

## Usage

``` r
mysterycall_progress_callback(total, label = "Processing")
```

## Arguments

- total:

  Total number of items

- label:

  Label for progress messages

## Value

A function that updates progress

## Examples

``` r
# \donttest{
progress <- mysterycall_progress_callback(100, "Processing")
for (i in 1:100) {
  # do work
  progress(i)
}
#>   ▸ Progress: 10/100 (10.0%) - Processing
#>   ▸ Progress: 20/100 (20.0%) - Processing
#>   ▸ Progress: 31/100 (31.0%) - Processing
#>   ▸ Progress: 42/100 (42.0%) - Processing
#>   ▸ Progress: 52/100 (52.0%) - Processing
#>   ▸ Progress: 63/100 (63.0%) - Processing
#>   ▸ Progress: 74/100 (74.0%) - Processing
#>   ▸ Progress: 85/100 (85.0%) - Processing
#>   ▸ Progress: 96/100 (96.0%) - Processing
#>   ▸ Progress: 100/100 (100.0%) - Processing
# }
```
