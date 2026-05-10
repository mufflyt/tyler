# ── mysterycall_check_generalist_presence ─────────────────────────────────────

make_presence_df <- function() {
  data.frame(
    city      = c("Denver","Denver","Denver","Austin","Austin","Austin"),
    state     = c("CO","CO","CO","TX","TX","TX"),
    specialty = c("General","Neurotology","Pediatric",
                  "Neurotology","Neurotology","General"),
    stringsAsFactors = FALSE
  )
}

test_that("check_generalist_presence: returns data frame", {
  df  <- make_presence_df()
  res <- mysterycall_check_generalist_presence(df, c("city","state"), "specialty", "General")
  expect_s3_class(res, "data.frame")
})

test_that("check_generalist_presence: expected column names", {
  df  <- make_presence_df()
  res <- mysterycall_check_generalist_presence(df, c("city","state"), "specialty", "General")
  expect_true(all(c("location","n_subspecialists","n_generalists","generalist_needed") %in% names(res)))
})

test_that("check_generalist_presence: Denver has generalist → not needed", {
  df  <- make_presence_df()
  res <- mysterycall_check_generalist_presence(df, c("city","state"), "specialty", "General")
  denver_row <- res[grepl("denver", tolower(res$location)), ]
  expect_false(denver_row$generalist_needed)
})

test_that("check_generalist_presence: Austin has generalist → not needed", {
  df  <- make_presence_df()
  res <- mysterycall_check_generalist_presence(df, c("city","state"), "specialty", "General")
  austin_row <- res[grepl("austin", tolower(res$location)), ]
  expect_false(austin_row$generalist_needed)
})

test_that("check_generalist_presence: location with only subspecialists → needed", {
  df <- data.frame(
    city      = c("Boulder","Boulder"),
    state     = c("CO","CO"),
    specialty = c("Neurotology","Pediatric"),
    stringsAsFactors = FALSE
  )
  res <- mysterycall_check_generalist_presence(df, c("city","state"), "specialty", "General")
  expect_equal(nrow(res), 1L)
  expect_true(res$generalist_needed[[1L]])
  expect_equal(res$n_generalists[[1L]], 0L)
})

test_that("check_generalist_presence: no subspecialists → 0 rows", {
  df <- data.frame(
    city      = "Denver",
    state     = "CO",
    specialty = "General",
    stringsAsFactors = FALSE
  )
  res <- mysterycall_check_generalist_presence(df, c("city","state"), "specialty", "General")
  expect_equal(nrow(res), 0L)
})

test_that("check_generalist_presence: case-insensitive city matching", {
  df <- data.frame(
    city      = c("DENVER","denver"),
    state     = c("CO","CO"),
    specialty = c("General","Neurotology"),
    stringsAsFactors = FALSE
  )
  res <- mysterycall_check_generalist_presence(df, c("city","state"), "specialty", "General")
  expect_equal(nrow(res), 1L)
  expect_false(res$generalist_needed[[1L]])
})

test_that("check_generalist_presence: missing location_col errors", {
  df <- data.frame(specialty = "General", stringsAsFactors = FALSE)
  expect_error(
    mysterycall_check_generalist_presence(df, c("city","state"), "specialty", "General"),
    "not found"
  )
})
