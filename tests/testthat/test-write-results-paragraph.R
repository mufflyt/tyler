make_irr_tbl <- function() {
  data.frame(
    term     = c("(Intercept)", "insuranceMedicaid", "insuranceUninsured"),
    irr      = c(1.00, 0.72, 0.55),
    ci_lower = c(NA,   0.60, 0.40),
    ci_upper = c(NA,   0.86, 0.75),
    p_value  = c(NA,   0.0003, 0.00000001),
    stringsAsFactors = FALSE
  )
}

test_that("returns a single character string", {
  res <- mysterycall_write_results_paragraph(make_irr_tbl(), "commercial insurance", "insurance")
  expect_type(res, "character")
  expect_length(res, 1L)
})

test_that("output contains the ref_group", {
  res <- mysterycall_write_results_paragraph(make_irr_tbl(), "commercial insurance", "insurance")
  expect_true(grepl("commercial insurance", res))
})

test_that("output contains exposure level names", {
  res <- mysterycall_write_results_paragraph(make_irr_tbl(), "commercial insurance", "insurance")
  expect_true(grepl("Medicaid",   res))
  expect_true(grepl("Uninsured",  res))
})

test_that("output contains 'IRR'", {
  res <- mysterycall_write_results_paragraph(make_irr_tbl(), "commercial insurance", "insurance")
  expect_true(grepl("IRR", res))
})

test_that("p < 0.001 formats as 'p < 0.001'", {
  res <- mysterycall_write_results_paragraph(make_irr_tbl(), "commercial insurance", "insurance")
  expect_true(grepl("p < 0.001", res))
})

test_that("works with data.frame input directly", {
  tbl <- make_irr_tbl()
  res <- mysterycall_write_results_paragraph(tbl, "commercial insurance", "insurance")
  expect_type(res, "character")
  expect_true(grepl("Medicaid", res))
})

test_that("errors on missing required column in irr_table", {
  bad <- make_irr_tbl()
  bad$p_value <- NULL
  expect_error(
    mysterycall_write_results_paragraph(bad, "commercial insurance", "insurance"),
    "p_value"
  )
})

test_that("errors when model_result is neither mysterycall_poisson_model nor data.frame", {
  expect_error(
    mysterycall_write_results_paragraph(list(foo = 1), "ref", "exposure"),
    "mysterycall_poisson_model"
  )
})

test_that("works with mysterycall_poisson_model object", {
  tbl <- make_irr_tbl()
  obj <- list(irr_table = tbl)
  class(obj) <- "mysterycall_poisson_model"
  res <- mysterycall_write_results_paragraph(obj, "commercial insurance", "insurance")
  expect_type(res, "character")
  expect_true(grepl("IRR", res))
})
