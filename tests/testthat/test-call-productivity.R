test_that("returns data.frame with correct columns", {
  df <- data.frame(
    caller = c("Alice","Alice","Bob","Bob","Bob"),
    stringsAsFactors = FALSE
  )
  res <- mysterycall_call_productivity(df, "caller")
  expect_s3_class(res, "data.frame")
  expected_cols <- c("caller","n_calls","n_days","calls_per_day",
                     "n_accepted","acceptance_rate","mean_hold_sec","mean_call_sec")
  expect_named(res, expected_cols)
})

test_that("n_calls per caller is correct", {
  df <- data.frame(
    caller = c("Alice","Alice","Bob","Bob","Bob","Carol"),
    stringsAsFactors = FALSE
  )
  res <- mysterycall_call_productivity(df, "caller")
  alice_row <- res[res$caller == "Alice", ]
  bob_row   <- res[res$caller == "Bob",   ]
  expect_equal(alice_row$n_calls, 2L)
  expect_equal(bob_row$n_calls,   3L)
})

test_that("date_col calculates n_days and calls_per_day", {
  df <- data.frame(
    caller = c("A","A","A","B","B"),
    date   = as.Date(c("2024-01-01","2024-01-01","2024-01-02","2024-01-01","2024-01-03")),
    stringsAsFactors = FALSE
  )
  res <- mysterycall_call_productivity(df, "caller", date_col = "date")
  a_row <- res[res$caller == "A", ]
  expect_equal(a_row$n_days, 2L)
  expect_equal(a_row$calls_per_day, 3 / 2)
})

test_that("outcome_col gives n_accepted and acceptance_rate", {
  df <- data.frame(
    caller  = c("A","A","A","B","B"),
    outcome = c(1, 0, 1, 0, 0),
    stringsAsFactors = FALSE
  )
  res <- mysterycall_call_productivity(df, "caller", outcome_col = "outcome")
  a_row <- res[res$caller == "A", ]
  expect_equal(a_row$n_accepted, 2)
  expect_equal(a_row$acceptance_rate, "66.7%")
})

test_that("result is sorted by n_calls descending", {
  df <- data.frame(
    caller = c("A","B","B","B","C","C"),
    stringsAsFactors = FALSE
  )
  res <- mysterycall_call_productivity(df, "caller")
  expect_true(all(diff(res$n_calls) <= 0))
})

test_that("errors on missing caller_col", {
  df <- data.frame(x = 1:3)
  expect_error(
    mysterycall_call_productivity(df, "caller"),
    "caller_col"
  )
})

test_that("errors when fewer than 2 unique callers", {
  df <- data.frame(caller = c("Alice","Alice"))
  expect_error(
    mysterycall_call_productivity(df, "caller"),
    "2 unique callers"
  )
})

test_that("MM:SS time format converts correctly", {
  df <- data.frame(
    caller    = c("A","A","B","B"),
    hold_time = c("01:30","02:00","00:45","03:15"),
    stringsAsFactors = FALSE
  )
  res <- mysterycall_call_productivity(df, "caller", hold_time_col = "hold_time")
  a_row <- res[res$caller == "A", ]
  # 1:30 = 90s, 2:00 = 120s, mean = 105s
  expect_equal(a_row$mean_hold_sec, 105)
})

test_that("total_calls_all attribute is set", {
  df <- data.frame(caller = c("A","A","B","B","B"))
  res <- mysterycall_call_productivity(df, "caller")
  expect_equal(attr(res, "total_calls_all"), 5L)
})
