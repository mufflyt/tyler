# Progress Bars Guide - Tyler Package

**Date:** 2025-10-29
**Version:** Added in tyler v1.2.3 (Week 3)

---

## Overview

The tyler package now includes **beautiful animated progress bars** powered by the `cli` package. Get real-time visual feedback with ETA calculations, status messages, and modern CLI UX.

---

## Quick Start

### Install CLI Package (Recommended)

```r
install.packages("cli")  # For beautiful animated progress bars
```

**Note:** Progress bars work WITHOUT cli (fallback to simple messages), but cli provides the best experience with animations, colors, and ETA calculations.

---

## Progress Bar Types

### 1. Simple Progress Bar

Perfect for single operations:

```r
library(tyler)

# Create progress bar
pb <- tyler_progress_bar("Processing records", total = 100)

# Update as you work
for (i in 1:100) {
  # Do your work here
  process_record(i)

  # Update progress
  tyler_progress_update(pb)
}

# Mark complete
tyler_progress_done(pb, result = "All records processed!")
```

**Output:**
```
⠋ Processing records ████████████████████░░░░░░░░ 75/100 (75%) ETA: 5s
```

---

### 2. Progress with Status Messages

Show what you're currently working on:

```r
pb <- tyler_progress_bar("Geocoding addresses", total = 50)

for (i in 1:50) {
  address <- addresses[i]
  coords <- geocode(address)

  tyler_progress_update(pb, status = address)
}

tyler_progress_done(pb, result = "50 addresses geocoded")
```

**Output:**
```
⠙ Geocoding addresses ████████████░░░░░░░░ 30/50 (60%) ETA: 12s
  Current: 123 Main St, Boston, MA
```

---

### 3. Multi-Step Workflows

For workflows with multiple major steps:

```r
# Define your workflow steps
tracker <- tyler_multi_progress(c(
  "Load Data",
  "Process Records",
  "Generate Output"
))

# Step 1
tyler_multi_step(tracker, 1, total = 100, detail = "Loading CSV files")
for (i in 1:100) {
  load_file(i)
  tyler_multi_update(tracker)
}
tyler_multi_complete(tracker, result = "100 files loaded")

# Step 2
tyler_multi_step(tracker, 2, total = 50, detail = "Processing data")
for (i in 1:50) {
  process(i)
  tyler_multi_update(tracker)
}
tyler_multi_complete(tracker)

# All done
tyler_multi_done(tracker)
```

**Output:**
```
━━ Step 1/3: Load Data ━━━━━━━━━━━━━━━━━━━━━━━━━━━
Loading CSV files

⠹ Load Data ████████████████████████ 100/100 (100%) Done
✔ 100 files loaded

━━ Step 2/3: Process Records ━━━━━━━━━━━━━━━━━━━━
Processing data

⠸ Process Records ████████████░░░░░░░░ 30/50 (60%) ETA: 8s
```

---

### 4. Functional Programming

Apply a function with automatic progress:

```r
# Process items with built-in progress tracking
results <- tyler_progress_map(
  items = addresses,
  fn = geocode_address,
  name = "Geocoding addresses",
  batch_size = 10  # Update every 10 items
)
```

**Output:**
```
⠹ Geocoding addresses ████████████████░░░░░░░░ 250/500 (50%) ETA: 2m
✔ Processed 500 items
```

---

### 5. Spinners (Indeterminate Operations)

For operations where you don't know the total:

```r
spinner_id <- tyler_spinner_start("Connecting to API")
# Do work with unknown duration
connect_to_service()
tyler_spinner_stop(spinner_id, result = "Connected!")
```

**Output:**
```
⠋ Working on: Connecting to API
✔ Connected!
```

---

## Advanced Features

### Custom Format Strings

Customize the progress bar appearance:

```r
pb <- tyler_progress_bar(
  "Custom bar",
  total = 100,
  format = "{cli::pb_spin} {name} [{cli::pb_bar}] {cli::pb_percent}"
)
```

### Error Handling

Handle failures gracefully:

```r
pb <- tyler_progress_bar("Processing", total = 100)

for (i in 1:100) {
  result <- tryCatch({
    process_item(i)
    tyler_progress_update(pb)
  }, error = function(e) {
    tyler_progress_fail(pb, msg = sprintf("Failed at item %d: %s", i, e$message))
    stop(e)
  })
}

tyler_progress_done(pb)
```

### Set Specific Values

Jump to a specific progress point:

```r
# Resume from where you left off
pb <- tyler_progress_bar("Resuming", total = 1000)
tyler_progress_update(pb, set = 750)  # Start at 750

# Continue from there
for (i in 751:1000) {
  process(i)
  tyler_progress_update(pb)
}
```

---

## Integration with Tyler Workflows

Progress bars integrate seamlessly with tyler's logging system:

```r
# Start workflow with logging
tyler_workflow_start("My Workflow", total_steps = 3)

# Step 1 with progress bar
tyler_log_step("Loading Data", n_items = 1000)
pb <- tyler_progress_bar("Loading files", total = 1000)
for (i in 1:1000) {
  load_file(i)
  tyler_progress_update(pb)
}
tyler_progress_done(pb)
tyler_log_step_complete(n_success = 1000, n_total = 1000)

# More steps...

tyler_workflow_end()
```

**You get BOTH:**
- ✅ Animated progress bars during work
- ✅ Structured log messages for audit trail
- ✅ Timing and success rates
- ✅ Professional output

---

## Best Practices

### 1. Choose the Right Tool

| Situation | Use |
|-----------|-----|
| Single operation | `tyler_progress_bar()` |
| Multi-step workflow | `tyler_multi_progress()` |
| Functional programming | `tyler_progress_map()` |
| Unknown duration | `tyler_spinner_start()` |

### 2. Update Frequency

```r
# ❌ Too frequent (slow)
for (i in 1:100000) {
  tyler_progress_update(pb)  # Updates 100K times!
}

# ✅ Smart batching
for (i in 1:100000) {
  if (i %% 1000 == 0) {  # Update every 1000 items
    tyler_progress_update(pb, set = i)
  }
}

# ✅ Or use batch_size parameter
tyler_progress_map(items, fn, batch_size = 1000)
```

### 3. Meaningful Status Messages

```r
# ❌ Not helpful
tyler_progress_update(pb, status = "Working")

# ✅ Specific and informative
tyler_progress_update(pb, status = sprintf("Geocoding: %s", address))
```

### 4. Always Complete

```r
# ❌ Leaves progress bar hanging
pb <- tyler_progress_bar("Task", total = 100)
for (i in 1:100) {
  tyler_progress_update(pb)
}
# Oops, forgot tyler_progress_done()!

# ✅ Always mark complete
pb <- tyler_progress_bar("Task", total = 100)
tryCatch({
  for (i in 1:100) {
    tyler_progress_update(pb)
  }
  tyler_progress_done(pb, result = "Success!")
}, error = function(e) {
  tyler_progress_fail(pb, msg = e$message)
  stop(e)
})
```

---

## Troubleshooting

### Q: Progress bar doesn't animate

**A:** You need the `cli` package for animations:
```r
install.packages("cli")
```

Without cli, you'll see simple message-based progress (which still works fine).

### Q: Progress bar shows weird characters

**A:** Your terminal doesn't support Unicode. The system will automatically fall back to simple ASCII output.

### Q: How do I disable progress bars?

**A:** Progress bars automatically disable in non-interactive mode or when output is redirected. To force disable:

```r
options(tyler.show_progress = FALSE)
```

### Q: Progress bars in Rmd/scripts?

**A:** Progress bars work best interactively. In R Markdown or scripts, they automatically fall back to message-based progress that shows in rendered output.

---

## Examples

### Example: NPI Search with Progress

```r
# Search with progress tracking
search_npi_with_progress <- function(names_df) {
  pb <- tyler_progress_bar("Searching NPI", total = nrow(names_df))

  results <- list()
  for (i in seq_len(nrow(names_df))) {
    name <- names_df[i, ]
    results[[i]] <- npi_search(name$first, name$last)
    tyler_progress_update(pb, status = paste(name$first, name$last))
  }

  tyler_progress_done(pb, result = sprintf("%d searches complete", nrow(names_df)))
  dplyr::bind_rows(results)
}
```

### Example: Complete Workflow

```r
run_complete_workflow <- function(input_file) {
  # Multi-step workflow
  tracker <- tyler_multi_progress(c(
    "Load & Validate",
    "NPI Search",
    "Geocoding",
    "Analysis"
  ))

  # Step 1
  tyler_multi_step(tracker, 1, total = 1)
  data <- read_csv(input_file)
  validate_data(data)
  tyler_multi_update(tracker)
  tyler_multi_complete(tracker, result = sprintf("%d records loaded", nrow(data)))

  # Step 2
  tyler_multi_step(tracker, 2, total = nrow(data))
  for (i in seq_len(nrow(data))) {
    data$npi[i] <- search_npi(data$first[i], data$last[i])
    tyler_multi_update(tracker)
  }
  tyler_multi_complete(tracker)

  # Step 3
  tyler_multi_step(tracker, 3, total = nrow(data))
  for (i in seq_len(nrow(data))) {
    data$coords[i] <- geocode(data$address[i])
    tyler_multi_update(tracker)
  }
  tyler_multi_complete(tracker)

  # Step 4
  tyler_multi_step(tracker, 4, total = 1)
  results <- analyze(data)
  tyler_multi_update(tracker)
  tyler_multi_complete(tracker)

  tyler_multi_done(tracker)
  results
}
```

---

## See Also

- `?tyler_progress_bar` - Main progress bar function
- `?tyler_multi_progress` - Multi-step tracker
- `?tyler_progress_map` - Functional programming
- `LOGGING_GUIDE.md` - Comprehensive logging
- `inst/examples/progress_bar_demo.R` - Live demo script

---

## Demo Script

Run the interactive demo to see all progress bar styles:

```r
source(system.file("examples/progress_bar_demo.R", package = "tyler"))
```

---

**Feedback?** Progress bars are part of Week 3 UX improvements. Report issues or suggestions on GitHub!
