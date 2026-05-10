skip_if_not_installed("DiagrammeR")

steps_basic <- c(
  "Directory"       = "671",
  "Phone Verified"  = "501",
  "Both Insurance"  = "432"
)

excl_basic <- c(
  "Phone Verified" = "170 excluded: no functioning phone",
  "Both Insurance" = "69 excluded: Medicaid not accepted"
)

# ── Basic functionality ────────────────────────────────────────────────────────

test_that("flowchart: returns an htmlwidget", {
  out <- mysterycall_flowchart(steps_basic)
  expect_true(inherits(out, "htmlwidget") || inherits(out, "grViz"))
})

test_that("flowchart: accepts exclusions without error", {
  expect_no_error(
    mysterycall_flowchart(steps_basic, exclusions = excl_basic)
  )
})

test_that("flowchart: accepts title without error", {
  expect_no_error(
    mysterycall_flowchart(steps_basic, title = "Study Flow")
  )
})

test_that("flowchart: single-step diagram works", {
  expect_no_error(
    mysterycall_flowchart(c("Final Sample" = "432"))
  )
})

# ── Input validation ──────────────────────────────────────────────────────────

test_that("flowchart: unnamed steps errors", {
  expect_error(mysterycall_flowchart(c("671", "501")), "named")
})

test_that("flowchart: empty steps errors", {
  expect_error(mysterycall_flowchart(character(0)), "named")
})

test_that("flowchart: exclusion name not in steps errors", {
  expect_error(
    mysterycall_flowchart(steps_basic, exclusions = c("No Such Step" = "10 excluded")),
    "No Such Step"
  )
})

test_that("flowchart: non-character steps errors", {
  expect_error(mysterycall_flowchart(1:3), "named character")
})

test_that("flowchart: unnamed exclusions errors", {
  expect_error(
    mysterycall_flowchart(steps_basic, exclusions = c("170 excluded")),
    "named"
  )
})
