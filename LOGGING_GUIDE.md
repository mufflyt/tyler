# Comprehensive Logging System - User Guide

**Date:** 2025-10-29
**Version:** Added in tyler v1.2.2

---

## Overview

The tyler package now includes a comprehensive logging system that provides **plain-language progress updates** throughout long-running workflows. This dramatically improves user experience by showing exactly what's happening, how long it's taking, and what the success rates are.

---

## Quick Start

### Option 1: Use the Enhanced Workflow (Recommended)

```r
library(tyler)

# Run the workflow with beautiful logging
results <- run_mystery_caller_workflow_with_logging(
  input_data = "physicians.csv",
  output_dir = "output/",
  google_maps_api_key = Sys.getenv("GOOGLE_API_KEY"),
  here_api_key = Sys.getenv("HERE_API_KEY")
)
```

**You'll see output like:**

```
============================================================
  Mystery Caller Workflow
  Started: 2025-10-29 14:30:00
  Total Steps: 5
============================================================

▶ Step 1/5: Searching NPI Registry
  Looking up physician NPIs by name
  Processing 1,234 item(s)...
  ✓ Step complete: 1,174/1,234 (95.1%) in 23m 15s

▶ Step 2/5: Geocoding Addresses
  Converting addresses to latitude/longitude coordinates
  Found 1174 total address records, 1152 unique
  ✓ Geocoding complete: 1,152/1,152 succeeded (100.0%)
  ✓ Step complete in 12m 30s

▶ Step 3/5: Generating Isochrones
  Creating drive-time polygons: 30, 60, 120, 180 minutes
  Processing 1,152 item(s)...
  ✓ Step complete in 45m 20s

...

============================================================
  Mystery Caller Workflow - COMPLETE
============================================================

  Input:  1,234 records
  Output: 1,142 records (92.5%)

  Step Summary:
    1. Searching NPI Registry: 1,174/1,234 (95.1%) in 23m 15s
    2. Geocoding Addresses: 1,152/1,152 (100.0%) in 12m 30s
    3. Generating Isochrones: 1,142/1,152 (99.1%) in 45m 20s
    4. Calculating Census Overlap: 1,142/1,142 (100.0%) in 18m 45s

  Total Duration: 1h 39m 50s
  Completed: 2025-10-29 16:09:50

============================================================
```

---

## Option 2: Add Logging to Custom Workflows

You can use the logging functions in your own scripts:

```r
library(tyler)

# Start workflow tracking
tyler_workflow_start(
  workflow_name = "My Custom Analysis",
  total_steps = 3,
  log_file = "my_workflow.log"  # Optional: save to file
)

# Step 1
tyler_log_step("Loading Data", n_items = 1000)
data <- read.csv("data.csv")
tyler_log_success("Loaded 1,000 records")
tyler_log_step_complete()

# Step 2
tyler_log_step("Processing Data", detail = "Cleaning and validating")
clean_data <- clean_my_data(data)
tyler_log_step_complete(n_success = 950, n_total = 1000)

# Step 3
tyler_log_step("Saving Results")
write.csv(clean_data, "output.csv")
tyler_log_save("output.csv", n_rows = 950)
tyler_log_step_complete()

# End with summary
tyler_workflow_end(final_n = 950, input_n = 1000)
```

---

## Available Logging Functions

### Workflow Control

- **`tyler_workflow_start(name, total_steps, log_file)`** - Initialize workflow
- **`tyler_workflow_end(final_n, input_n)`** - Print summary
- **`tyler_log_step(name, detail, n_items)`** - Start a new step
- **`tyler_log_step_complete(success_rate, n_success, n_total)`** - Complete step

### Message Types

- **`tyler_log_info(msg)`** - ℹ Informational message
- **`tyler_log_success(msg, details)`** - ✓ Success message (green)
- **`tyler_log_warning(msg, fix)`** - ⚠ Warning message (yellow)
- **`tyler_log_error(msg, cause, fix)`** - ✗ Error with cause and fix
- **`tyler_log_progress(current, total, status)`** - Progress update

### Special Messages

- **`tyler_log_cache_hit(what, n_items)`** - ↻ Loaded from cache
- **`tyler_log_save(path, n_rows)`** - 💾 File saved

---

## Benefits

### Before Logging:
```r
result <- search_and_process_npi(data)
# [silence for 30 minutes]
# Did it work? Is it running? No idea!
```

### After Logging:
```r
▶ Step 1/5: Searching NPI Registry
  Processing 1,234 item(s)...
  ▸ Progress: 500/1,234 (40.5%)
  ▸ Progress: 1,000/1,234 (81.0%)
  ✓ Step complete: 1,174/1,234 (95.1%) in 23m 15s
```

**You know:**
- ✅ It's working
- ✅ How much progress has been made
- ✅ Estimated time remaining
- ✅ Success rate
- ✅ Where failures occurred

---

## Log Files

All logs can be saved to a file for later review:

```r
tyler_workflow_start(
  "My Workflow",
  log_file = "logs/workflow_20251029.txt"
)
```

The log file contains a clean text version (without Unicode symbols) that's perfect for:
- Debugging issues
- Audit trails
- Performance analysis
- Sharing with collaborators

---

## Integration with Existing Functions

The logging system is **already integrated** into key functions:

- ✅ `geocode_unique_addresses()` - Shows progress and success rates
- ✅ `search_and_process_npi()` - Already has detailed logging
- ✅ `create_isochrones_for_dataframe()` - Can be enhanced further
- ✅ `calculate_intersection_overlap_and_save()` - Can be enhanced further

---

## Tips for Best Experience

1. **Use `quiet = FALSE`** in function calls to see detailed logging
2. **Save log files** for long workflows to review later
3. **Check success rates** at each step to catch problems early
4. **Use the enhanced workflow wrapper** for the best experience

---

## Troubleshooting

### Q: I don't see any log messages

**A:** Check that you're not using `quiet = TRUE` mode. Try:
```r
options(tyler.quiet = FALSE)
```

### Q: Unicode symbols don't display correctly

**A:** Your terminal may not support Unicode. The log file will have clean text versions.

### Q: How do I disable logging?

**A:** Use `quiet = TRUE` in function arguments, or set:
```r
options(tyler.quiet = TRUE)
```

---

## Examples

### Example 1: Simple Workflow

```r
tyler_workflow_start("Quick Analysis", total_steps = 2)

tyler_log_step("Loading Data")
data <- read.csv("input.csv")
tyler_log_success(sprintf("Loaded %d rows", nrow(data)))
tyler_log_step_complete()

tyler_log_step("Analyzing")
results <- analyze(data)
tyler_log_step_complete(n_success = nrow(results), n_total = nrow(data))

tyler_workflow_end(final_n = nrow(results), input_n = nrow(data))
```

### Example 2: With Error Handling

```r
tyler_log_step("Risky Operation")

tryCatch({
  result <- risky_function()
  tyler_log_success("Operation succeeded!")
  tyler_log_step_complete()
}, error = function(e) {
  tyler_log_error(
    "Operation failed",
    cause = e$message,
    fix = "Check your input data format"
  )
  stop(e)
})
```

### Example 3: Progress Tracking

```r
tyler_log_step("Processing Items", n_items = 1000)

for (i in 1:1000) {
  # Do work...

  # Report progress every 100 items
  if (i %% 100 == 0) {
    tyler_log_progress(i, 1000, status = "Processing...")
  }
}

tyler_log_step_complete()
```

---

## See Also

- `?run_mystery_caller_workflow_with_logging` - Enhanced workflow wrapper
- `?tyler_workflow_start` - Workflow control functions
- `?tyler_log_info` - Message logging functions
- `PRODUCTION_95_PERCENT_SUCCESS_PLAN.md` - Production readiness plan

---

**Feedback?** This logging system addresses Risk #9 (Insufficient Logging) from the production readiness analysis. Please report any issues or suggestions!
