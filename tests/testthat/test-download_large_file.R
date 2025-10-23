test_that("download_large_file returns existing complete file", {
  tmp_dir <- tempdir()
  dest <- normalizePath(file.path(tmp_dir, "example.dat"), mustWork = FALSE)
  writeBin(raw(10), dest)

  mockery::stub(download_large_file, "get_content_length", function(...) 10)
  expect_equal(download_large_file("https://example.com/data", dest, overwrite = FALSE), dest)
})

test_that("download_large_file detects in-progress lock", {
  tmp_dir <- tempdir()
  dest <- normalizePath(file.path(tmp_dir, "locked.dat"), mustWork = FALSE)
  lock_path <- paste0(dest, ".lock")
  dir.create(dirname(lock_path), recursive = TRUE, showWarnings = FALSE)
  file.create(lock_path)

  mockery::stub(download_large_file, "get_content_length", function(...) NA_real_)
  expect_equal(download_large_file("https://example.com/data", dest, overwrite = FALSE), dest)
  unlink(lock_path)
})

test_that("download_large_file falls back through download methods", {
  tmp_dir <- tempdir()
  dest <- normalizePath(file.path(tmp_dir, "fallback.dat"), mustWork = FALSE)
  dest_tmp <- paste0(dest, ".download")

  calls <- character(0)

  mockery::stub(download_large_file, "get_content_length", function(...) 4)
  mockery::stub(download_large_file, "download_with_wget", function(url, dest, quiet) {
    calls <<- c(calls, "wget")
    FALSE
  })
  mockery::stub(download_large_file, "download_with_curl", function(url, dest, quiet) {
    calls <<- c(calls, "curl")
    FALSE
  })
  mockery::stub(download_large_file, "download_with_download_file", function(url, dest, quiet) {
    calls <<- c(calls, "download.file")
    writeBin(as.raw(rep(0, 4)), dest)
    TRUE
  })

  result <- download_large_file("https://example.com/data", dest, overwrite = TRUE)
  expect_equal(result, normalizePath(dest, mustWork = FALSE))
  expect_true(file.exists(result))
  expect_equal(file.info(result)$size, 4)
  expect_equal(calls, c("wget", "curl", "download.file"))

  unlink(c(dest, dest_tmp))
})
