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
