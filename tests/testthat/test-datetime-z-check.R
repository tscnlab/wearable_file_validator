library(testthat)

load_app_functions <- function(path = "app.R") {
  lines <- readLines(path, warn = FALSE)
  lines <- lines[!grepl("^shinyApp\\(ui, server\\)\\s*$", lines)]
  env <- new.env(parent = globalenv())
  eval(parse(text = paste(lines, collapse = "\n")), envir = env)
  env
}

test_that("fraction_has_utc_z handles uppercase and lowercase Z suffixes", {
  app_env <- load_app_functions()

  values <- c("2026-03-09T08:00:00Z", "2026-03-09T08:01:00z", "2026-03-09T08:02:00Z")
  expect_equal(app_env$fraction_has_utc_z(values), 1)
})

test_that("extract_datetime_raw_z_fraction reads Datetime directly from raw lines", {
  app_env <- load_app_functions()

  lines <- c(
    "Datetime,device_id",
    "2026-03-09T08:00:00Z,DEV001",
    "2026-03-09T08:01:00z,DEV001",
    "2026-03-09T08:02:00Z,DEV001"
  )

  z_info <- app_env$extract_datetime_raw_z_fraction(lines)
  expect_equal(z_info$fraction, 1)
  expect_equal(z_info$n_non_missing, 3)
  expect_equal(z_info$col_ix, 1)
})

test_that("UTC suffix check uses raw lines when Datetime is parsed POSIXct", {
  app_env <- load_app_functions()

  dat <- data.frame(
    Datetime = as.POSIXct(c("2026-03-09 08:00:00", "2026-03-09 08:01:00"), tz = "UTC"),
    value = c(1, 2)
  )
  lines <- c(
    "Datetime,value",
    "2026-03-09T08:00:00Z,1",
    "2026-03-09T08:01:00Z,2"
  )

  checks <- app_env$validate_level_7_content(dat, lines = lines)
  utc_check <- checks[[which(vapply(checks, `[[`, character(1), "id") == "utc_suffix")]]

  expect_equal(utc_check$status, "pass")
  expect_match(utc_check$message, "in the raw file")
})

test_that("detect_header_skip identifies preamble lines before header", {
  app_env <- load_app_functions()

  lines <- c(
    "# Exported from Device Vendor X",
    "# Site: ExampleLab",
    "Datetime,device_id,value",
    "2026-03-09T08:00:00Z,DEV001,1"
  )

  detected <- app_env$detect_header_skip(lines)
  expect_equal(detected$skip, 2)
  expect_match(detected$reason, "line 3")
})

test_that("run_all_validations supports manual skip before header", {
  app_env <- load_app_functions()

  tf <- tempfile(fileext = ".csv")
  writeLines(c(
    "# created by tracker utility",
    "Datetime,value",
    "2026-03-09T08:00:00Z,1",
    "2026-03-09T08:01:00Z,2"
  ), tf)

  results <- app_env$run_all_validations(tf, skip_lines = 1, auto_detect_skip = FALSE)

  expect_equal(results$skip_lines, 1)
  expect_true("Datetime" %in% names(results$data))
})


test_that("extract_datetime_raw_z_fraction handles quoted commas in prior columns", {
  app_env <- load_app_functions()

  lines <- c(
    "device_note,Datetime,value",
    '"walk, outdoors",2026-03-09T08:00:00Z,1',
    '"run, indoors",2026-03-09T08:01:00,2'
  )

  z_info <- app_env$extract_datetime_raw_z_fraction(lines)
  expect_equal(z_info$n_non_missing, 2)
  expect_equal(z_info$fraction, 0.5)
})

test_that("UTC suffix check reports preview-only info for truncated raw lines", {
  app_env <- load_app_functions()

  dat <- data.frame(
    Datetime = as.POSIXct(c("2026-03-09 08:00:00", "2026-03-09 08:01:00"), tz = "UTC"),
    value = c(1, 2)
  )
  lines <- c(
    "Datetime,value",
    "2026-03-09T08:00:00Z,1",
    "2026-03-09T08:01:00,2"
  )

  checks <- app_env$validate_level_7_content(dat, lines = lines, lines_is_sample = TRUE)
  utc_check <- checks[[which(vapply(checks, `[[`, character(1), "id") == "utc_suffix")]]

  expect_equal(utc_check$status, "info")
  expect_match(utc_check$message, "preview-only")
})

test_that("build_data_preview keeps POSIXct display human-readable", {
  app_env <- load_app_functions()

  dat <- data.frame(
    Datetime = as.POSIXct(c("2026-03-09 08:00:00", NA), tz = "UTC"),
    value = c(1, 2)
  )

  preview <- app_env$build_data_preview(dat, n = 2)

  expect_type(preview$Datetime, "character")
  expect_equal(preview$Datetime[1], "2026-03-09 08:00:00 UTC")
  expect_true(is.na(preview$Datetime[2]))
})
