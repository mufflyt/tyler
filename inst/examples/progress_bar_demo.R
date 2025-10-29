#' Progress Bar Demonstration Script
#'
#' This script demonstrates the beautiful progress bar system in the tyler package.
#' Shows various progress bar styles and use cases.
#'
#' Run this interactively to see animated progress bars!

library(tyler)

# Make sure cli is installed for best experience
if (!requireNamespace("cli", quietly = TRUE)) {
  message("For best experience, install cli package:")
  message("  install.packages('cli')")
  message("\nFalling back to simple progress display...\n")
}

message("")
message("╭─────────────────────────────────────────────────────────────╮")
message("│  Tyler Package - Progress Bar Demonstration                │")
message("╰─────────────────────────────────────────────────────────────╯")
message("")

# ==================== Demo 1: Simple Progress Bar ====================
message("\n📊 Demo 1: Simple Progress Bar\n")

pb <- tyler_progress_bar("Processing records", total = 50)
for (i in 1:50) {
  Sys.sleep(0.05)  # Simulate work
  tyler_progress_update(pb)
}
tyler_progress_done(pb, result = "All records processed!")

Sys.sleep(1)

# ==================== Demo 2: Progress with Status Messages ====================
message("\n📊 Demo 2: Progress with Status Updates\n")

pb <- tyler_progress_bar("Geocoding addresses", total = 30)
for (i in 1:30) {
  address <- sprintf("Address %d", i)
  Sys.sleep(0.1)  # Simulate geocoding
  tyler_progress_update(pb, status = address)
}
tyler_progress_done(pb, result = "30 addresses geocoded successfully")

Sys.sleep(1)

# ==================== Demo 3: Progress with Failures ====================
message("\n📊 Demo 3: Progress with Error Handling\n")

pb <- tyler_progress_bar("Validating API keys", total = 5)
for (i in 1:5) {
  Sys.sleep(0.3)
  if (i == 3) {
    # Simulate failure
    tyler_progress_fail(pb, msg = "API key #3 invalid")
    break
  }
  tyler_progress_update(pb, status = sprintf("Testing key #%d", i))
}

Sys.sleep(1)

# ==================== Demo 4: Multi-Step Progress ====================
message("\n📊 Demo 4: Multi-Step Workflow\n")

tracker <- tyler_multi_progress(
  steps = c("Load Data", "Process Records", "Generate Output")
)

# Step 1
tyler_multi_step(tracker, 1, total = 20, detail = "Loading CSV files...")
for (i in 1:20) {
  Sys.sleep(0.05)
  tyler_multi_update(tracker)
}
tyler_multi_complete(tracker, result = "20 files loaded")

Sys.sleep(0.5)

# Step 2
tyler_multi_step(tracker, 2, total = 15, detail = "Processing and validating...")
for (i in 1:15) {
  Sys.sleep(0.08)
  tyler_multi_update(tracker, status = sprintf("Record %d", i))
}
tyler_multi_complete(tracker, result = "15 records validated")

Sys.sleep(0.5)

# Step 3
tyler_multi_step(tracker, 3, total = 10, detail = "Generating reports...")
for (i in 1:10) {
  Sys.sleep(0.1)
  tyler_multi_update(tracker)
}
tyler_multi_complete(tracker, result = "Reports generated")

tyler_multi_done(tracker)

Sys.sleep(1)

# ==================== Demo 5: Progress Map ====================
message("\n📊 Demo 5: Functional Programming with Progress\n")

# Process items with automatic progress tracking
results <- tyler_progress_map(
  items = 1:25,
  fn = function(x) {
    Sys.sleep(0.08)  # Simulate work
    x^2  # Return result
  },
  name = "Computing squares"
)

message(sprintf("Computed %d results", length(results)))

Sys.sleep(1)

# ==================== Demo 6: Spinner for Indeterminate Operations ====================
message("\n📊 Demo 6: Spinner for Unknown Duration\n")

spinner_id <- tyler_spinner_start("Connecting to API", msg = "Establishing connection...")
Sys.sleep(2)  # Simulate network operation
tyler_spinner_stop(spinner_id, result = "Connected!")

Sys.sleep(1)

# ==================== Summary ====================
message("")
message("╭─────────────────────────────────────────────────────────────╮")
message("│  ✨ Progress Bar Demo Complete!                            │")
message("╰─────────────────────────────────────────────────────────────╯")
message("")
message("These progress bars work with:")
message("  • tyler_progress_bar() - Single operation progress")
message("  • tyler_multi_progress() - Multi-step workflows")
message("  • tyler_progress_map() - Functional programming")
message("  • tyler_spinner_start() - Indeterminate operations")
message("")
message("All integrate seamlessly with tyler's logging system!")
message("")
message("Try them in your own workflows:")
message("  ?tyler_progress_bar")
message("  ?tyler_multi_progress")
message("")
