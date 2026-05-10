# Create a beautiful progress bar

Creates an animated progress bar with ETA, percentage, and custom
formatting. Uses the cli package for cross-platform compatibility and
beautiful output.

## Usage

``` r
mysterycall_progress_bar(
  name,
  total,
  format = NULL,
  clear = FALSE,
  show_after = 0,
  force = FALSE
)
```

## Arguments

- name:

  Name/description of the operation

- total:

  Total number of items to process

- format:

  Custom format string (uses cli::cli_progress_bar format)

- clear:

  Whether to clear the progress bar when done (default: FALSE)

- show_after:

  Show progress bar after this many seconds (default: 0)

- force:

  Whether to force progress bar even if not in terminal (default: FALSE)

## Value

Progress bar ID (invisible)

## Examples

``` r
# \donttest{
# Simple progress bar
pb_id <- mysterycall_progress_bar("Processing", total = 100)
#> Starting: Processing (100 items)
for (i in 1:100) {
  Sys.sleep(0.1)
  mysterycall_progress_update(pb_id)
}
#>   Progress: 10/100 (10%) - ETA: 9.0s
#>   Progress: 20/100 (20%) - ETA: 8.0s
#>   Progress: 30/100 (30%) - ETA: 7.0s
#>   Progress: 40/100 (40%) - ETA: 6.1s
#>   Progress: 50/100 (50%) - ETA: 5.0s
#>   Progress: 60/100 (60%) - ETA: 4.0s
#>   Progress: 70/100 (70%) - ETA: 3.0s
#>   Progress: 80/100 (80%) - ETA: 2.0s
#>   Progress: 90/100 (90%) - ETA: 1.0s
#>   Progress: 100/100 (100%) - ETA: 0.0s
mysterycall_progress_done(pb_id)
#>   ✓ Processing complete

# With custom message
pb_id <- mysterycall_progress_bar("Geocoding addresses", total = 500)
#> Starting: Geocoding addresses (500 items)
for (i in 1:500) {
  # do work
  mysterycall_progress_update(pb_id, status = sprintf("Address %d", i))
}
#>   Progress: 48/500 (10%) - ETA: 0.0s
#>   Progress: 98/500 (20%) - ETA: 0.0s
#>   Progress: 148/500 (30%) - ETA: 0.0s
#>   Progress: 198/500 (40%) - ETA: 0.0s
#>   Progress: 248/500 (50%) - ETA: 0.0s
#>   Progress: 298/500 (60%) - ETA: 0.0s
#>   Progress: 348/500 (70%) - ETA: 0.0s
#>   Progress: 398/500 (80%) - ETA: 0.0s
#>   Progress: 448/500 (90%) - ETA: 0.0s
#>   Progress: 498/500 (100%) - ETA: 0.0s
#>   Progress: 500/500 (100%) - ETA: 0.0s
mysterycall_progress_done(pb_id, result = "All addresses geocoded!")
#>   ✓ All addresses geocoded!
# }
```
