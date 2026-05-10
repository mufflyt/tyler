skip_if_not_installed("bizdays")

# ── mysterycall_us_federal_calendar ───────────────────────────────────────────

test_that("calendar: returns a bizdays calendar object", {
  cal <- mysterycall_us_federal_calendar(2025, 2026)
  expect_true(inherits(cal, "Calendar"))
})

test_that("calendar: rejects end_year < start_year", {
  expect_error(mysterycall_us_federal_calendar(2026, 2024), "end_year")
})

test_that("calendar: rejects non-scalar start_year", {
  expect_error(mysterycall_us_federal_calendar(c(2024, 2025), 2026), "single number")
})

test_that("calendar: MLK Day 2026 is not a business day", {
  # MLK Day 2026 = Mon Jan 19
  cal <- mysterycall_us_federal_calendar(2025, 2027)
  expect_false(bizdays::is.bizday(as.Date("2026-01-19"), cal))
})

test_that("calendar: day before MLK Day 2026 is a business day", {
  cal <- mysterycall_us_federal_calendar(2025, 2027)
  expect_true(bizdays::is.bizday(as.Date("2026-01-16"), cal))  # Friday
})

test_that("calendar: Presidents Day 2026 is not a business day", {
  # Presidents Day 2026 = Mon Feb 16
  cal <- mysterycall_us_federal_calendar(2025, 2027)
  expect_false(bizdays::is.bizday(as.Date("2026-02-16"), cal))
})

test_that("calendar: Memorial Day 2026 is not a business day", {
  # Memorial Day 2026 = Mon May 25
  cal <- mysterycall_us_federal_calendar(2025, 2027)
  expect_false(bizdays::is.bizday(as.Date("2026-05-25"), cal))
})

test_that("calendar: Labor Day 2026 is not a business day", {
  # Labor Day 2026 = Mon Sep 7
  cal <- mysterycall_us_federal_calendar(2025, 2027)
  expect_false(bizdays::is.bizday(as.Date("2026-09-07"), cal))
})

test_that("calendar: Thanksgiving 2026 is not a business day", {
  # Thanksgiving 2026 = Thu Nov 26
  cal <- mysterycall_us_federal_calendar(2025, 2027)
  expect_false(bizdays::is.bizday(as.Date("2026-11-26"), cal))
})

test_that("calendar: Christmas 2026 is not a business day", {
  # Christmas 2026 = Fri Dec 25
  cal <- mysterycall_us_federal_calendar(2025, 2027)
  expect_false(bizdays::is.bizday(as.Date("2026-12-25"), cal))
})

test_that("calendar: July 4 on Saturday shifts to Friday", {
  # July 4, 2026 = Saturday → observed holiday on Friday July 3
  cal <- mysterycall_us_federal_calendar(2025, 2027)
  expect_false(bizdays::is.bizday(as.Date("2026-07-03"), cal))  # Friday observed holiday
  expect_false(bizdays::is.bizday(as.Date("2026-07-04"), cal))  # Saturday (weekend)
  expect_true(bizdays::is.bizday(as.Date("2026-07-02"), cal))   # Thursday before — normal bizday
})

test_that("calendar: New Year's Day on Sunday shifts to Monday", {
  # Jan 1, 2023 = Sunday → observed Mon Jan 2
  cal <- mysterycall_us_federal_calendar(2022, 2024)
  expect_false(bizdays::is.bizday(as.Date("2023-01-02"), cal))
  expect_true(bizdays::is.bizday(as.Date("2023-01-03"), cal))
})

test_that("calendar: Juneteenth 2025 is not a business day", {
  # Jun 19, 2025 = Thursday
  cal <- mysterycall_us_federal_calendar(2024, 2026)
  expect_false(bizdays::is.bizday(as.Date("2025-06-19"), cal))
})

test_that("calendar: Veterans Day 2025 is not a business day", {
  # Nov 11, 2025 = Tuesday
  cal <- mysterycall_us_federal_calendar(2024, 2026)
  expect_false(bizdays::is.bizday(as.Date("2025-11-11"), cal))
})

# ── mysterycall_count_business_days ──────────────────────────────────────────

test_that("count_bdays: Mon-to-Fri same week = 4", {
  # 2026-02-02 Mon to 2026-02-06 Fri, no holidays
  expect_equal(mysterycall_count_business_days("2026-02-02", "2026-02-06"), 4L)
})

test_that("count_bdays: same date = 0", {
  expect_equal(mysterycall_count_business_days("2026-03-10", "2026-03-10"), 0L)
})

test_that("count_bdays: spans Presidents Day = one fewer day", {
  # 2026-02-13 Fri to 2026-02-20 Fri: 5 business days but Presidents Day (Mon Feb 16) excluded → 4
  expect_equal(mysterycall_count_business_days("2026-02-13", "2026-02-20"), 4L)
})

test_that("count_bdays: NA start_date returns NA", {
  expect_true(is.na(mysterycall_count_business_days(NA, "2026-03-10")))
})

test_that("count_bdays: NA end_date returns NA", {
  expect_true(is.na(mysterycall_count_business_days("2026-03-10", NA)))
})

test_that("count_bdays: end < start returns NA with warning", {
  expect_warning(
    result <- mysterycall_count_business_days("2026-03-10", "2026-03-01"),
    "appointment_date < call_date"
  )
  expect_true(is.na(result))
})

test_that("count_bdays: vectorized — returns integer vector of correct length", {
  starts <- c("2026-02-02", "2026-02-09", "2026-02-16")
  ends   <- c("2026-02-06", "2026-02-13", "2026-02-20")
  res    <- mysterycall_count_business_days(starts, ends)
  expect_length(res, 3L)
  expect_type(res, "integer")
})

test_that("count_bdays: mixed NA vector handled element-wise", {
  starts <- c("2026-02-02", NA,           "2026-02-16")
  ends   <- c("2026-02-06", "2026-02-13", "2026-02-20")
  res    <- mysterycall_count_business_days(starts, ends)
  expect_equal(res[[1L]], 4L)
  expect_true(is.na(res[[2L]]))
  expect_false(is.na(res[[3L]]))
})

test_that("count_bdays: scalar start recycled to match vector end", {
  res <- mysterycall_count_business_days("2026-02-02",
                                         c("2026-02-03", "2026-02-04"))
  expect_length(res, 2L)
  expect_equal(res, c(1L, 2L))
})

test_that("count_bdays: scalar end recycled to match vector start", {
  res <- mysterycall_count_business_days(c("2026-02-02", "2026-02-03"),
                                         "2026-02-06")
  expect_length(res, 2L)
  expect_equal(res, c(4L, 3L))
})

test_that("count_bdays: mismatched non-scalar lengths error", {
  expect_error(
    mysterycall_count_business_days(c("2026-02-02", "2026-02-03"),
                                    c("2026-02-06", "2026-02-07", "2026-02-08")),
    "same length"
  )
})

test_that("count_bdays: accepts Date objects directly", {
  start <- as.Date("2026-02-02")
  end   <- as.Date("2026-02-06")
  expect_equal(mysterycall_count_business_days(start, end), 4L)
})

test_that("count_bdays: pre-built calendar is reused correctly", {
  cal <- mysterycall_us_federal_calendar(2025, 2027)
  r1  <- mysterycall_count_business_days("2026-02-02", "2026-02-06", calendar = cal)
  r2  <- mysterycall_count_business_days("2026-02-02", "2026-02-06")
  expect_equal(r1, r2)
})

# ── mysterycall_business_days ─────────────────────────────────────────────────

test_that("business_days: adds result column to data frame", {
  df <- data.frame(
    call_date        = as.Date(c("2026-02-02", "2026-02-09")),
    appointment_date = as.Date(c("2026-02-06", "2026-02-13"))
  )
  out <- mysterycall_business_days(df)
  expect_true("business_days_until_appointment" %in% names(out))
  expect_equal(out$business_days_until_appointment, c(4L, 4L))
})

test_that("business_days: custom column names work", {
  df <- data.frame(
    call   = as.Date("2026-02-02"),
    appt   = as.Date("2026-02-06"),
    stringsAsFactors = FALSE
  )
  out <- mysterycall_business_days(df, call_col = "call", appt_col = "appt",
                                    result_col = "bdays")
  expect_true("bdays" %in% names(out))
  expect_equal(out$bdays, 4L)
})

test_that("business_days: preserves all original columns", {
  df <- data.frame(
    physician        = "Dr. A",
    call_date        = as.Date("2026-02-02"),
    appointment_date = as.Date("2026-02-06"),
    stringsAsFactors = FALSE
  )
  out <- mysterycall_business_days(df)
  expect_true(all(c("physician", "call_date", "appointment_date",
                     "business_days_until_appointment") %in% names(out)))
})

test_that("business_days: NA call_date produces NA result with message", {
  df <- data.frame(
    call_date        = as.Date(c(NA, "2026-02-09")),
    appointment_date = as.Date(c("2026-02-06", "2026-02-13"))
  )
  expect_message(out <- mysterycall_business_days(df), "NA")
  expect_true(is.na(out$business_days_until_appointment[[1L]]))
  expect_false(is.na(out$business_days_until_appointment[[2L]]))
})

test_that("business_days: result_col same as call_col errors", {
  df <- data.frame(
    call_date        = as.Date("2026-02-02"),
    appointment_date = as.Date("2026-02-06")
  )
  expect_error(
    mysterycall_business_days(df, result_col = "call_date"),
    "call_col"
  )
})

test_that("business_days: missing call_col errors", {
  df <- data.frame(appointment_date = as.Date("2026-02-06"))
  expect_error(mysterycall_business_days(df), "call_date")
})

test_that("business_days: missing appt_col errors", {
  df <- data.frame(call_date = as.Date("2026-02-02"))
  expect_error(mysterycall_business_days(df), "appointment_date")
})

test_that("business_days: non-character result_col errors", {
  df <- data.frame(
    call_date        = as.Date("2026-02-02"),
    appointment_date = as.Date("2026-02-06")
  )
  expect_error(mysterycall_business_days(df, result_col = 42), "non-empty character")
})

test_that("business_days: spans Presidents Day 2026", {
  df <- data.frame(
    call_date        = as.Date("2026-02-13"),
    appointment_date = as.Date("2026-02-20")
  )
  out <- mysterycall_business_days(df)
  expect_equal(out$business_days_until_appointment, 4L)
})
