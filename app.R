library(shiny)
library(vroom)
library(tools)

options(shiny.maxRequestSize = 20 * 1024^2)

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

escape_html <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x
}

syntactic_name_ok <- function(x) {
  if (is.na(x) || !nzchar(x)) return(FALSE)
  make.names(x) == x
}

is_unique <- function(x) length(unique(x)) == length(x)

guess_separator_from_header <- function(header_line) {
  seps <- c("," = ",", ";" = ";", "\t" = "\t", "|" = "|")
  counts <- vapply(seps, function(s) {
    if (s == "\t") {
      lengths(strsplit(header_line, "\t", fixed = TRUE)) - 1L
    } else {
      lengths(strsplit(header_line, s, fixed = TRUE)) - 1L
    }
  }, integer(1))
  names(counts)[which.max(counts)]
}

safe_read_lines <- function(path, n = 2000L) {
  is_gz <- identical(tolower(file_ext(path)), "gz")
  con <- if (is_gz) gzfile(path, open = "rt") else file(path, open = "rb")
  on.exit(close(con), add = TRUE)
  readLines(con, n = n, warn = FALSE, encoding = "UTF-8")
}

parse_delimited_line <- function(line, delim = ",") {
  if (is.na(line)) return(character(0))

  parsed <- tryCatch(
    utils::read.table(
      text = line,
      sep = delim,
      quote = '"',
      header = FALSE,
      fill = TRUE,
      comment.char = "",
      stringsAsFactors = FALSE,
      colClasses = "character"
    ),
    error = function(e) NULL
  )

  if (is.null(parsed)) {
    return(strsplit(line, delim, fixed = TRUE)[[1]])
  }

  vals <- as.character(parsed[1, , drop = TRUE])
  vals[is.na(vals)] <- ""
  vals
}

safe_read_vroom <- function(path, delim = ",", n_max = Inf, altrep = TRUE, skip = 0L) {
  vroom::vroom(
    file = path,
    delim = delim,
    skip = skip,
    altrep = altrep,
    show_col_types = FALSE,
    progress = FALSE,
    na = c("", "NA", "N/A", "NULL", "null"),
    trim_ws = FALSE,
    num_threads = max(1, parallel::detectCores(logical = TRUE) - 1),
    n_max = n_max
  )
}

detect_header_skip <- function(lines, delim = ",", required_col = "Datetime", max_scan = 100L) {
  if (length(lines) == 0) {
    return(list(skip = 0L, header_line = NA_character_, reason = "No lines available."))
  }

  scan_n <- min(length(lines), as.integer(max_scan))
  candidate_lines <- lines[seq_len(scan_n)]

  split_line <- function(line) parse_delimited_line(line, delim = delim)
  parts <- lapply(candidate_lines, split_line)

  clean_fields <- lapply(parts, function(x) trimws(gsub('^"|"$', "", x)))

  score_line <- function(fields) {
    if (length(fields) < 2) return(-Inf)

    non_missing <- fields[nzchar(fields)]
    has_required <- required_col %in% fields
    unique_non_missing <- length(unique(non_missing)) == length(non_missing)
    syntactic_fraction <- if (length(non_missing) == 0) 0 else mean(vapply(non_missing, syntactic_name_ok, logical(1)))

    (if (has_required) 3 else 0) +
      (if (unique_non_missing) 1.5 else 0) +
      syntactic_fraction +
      min(length(fields) / 10, 2)
  }

  scores <- vapply(clean_fields, score_line, numeric(1))
  valid <- which(is.finite(scores))

  if (length(valid) == 0) {
    return(list(skip = 0L, header_line = lines[1], reason = "No plausible header detected; defaulted to first line."))
  }

  best_ix <- valid[which.max(scores[valid])]
  list(
    skip = as.integer(best_ix - 1L),
    header_line = lines[best_ix],
    reason = if (best_ix == 1L) "First line appears to be the header." else paste0("Detected likely header on line ", best_ix, ".")
  )
}

skip_preamble_lines <- function(lines, skip = 0L) {
  skip <- max(0L, as.integer(skip))
  if (skip >= length(lines)) return(character(0))
  lines[seq.int(skip + 1L, length(lines))]
}

parse_posix_utc <- function(x) {
  suppressWarnings(as.POSIXct(x, tz = "UTC", format = "%Y-%m-%dT%H:%M:%OSZ"))
}

fraction_has_utc_z <- function(x) {
  vals <- as.character(x)
  vals <- trimws(vals)
  vals <- vals[!is.na(vals) & nzchar(vals)]
  if (length(vals) == 0) return(NA_real_)
  mean(grepl("(?:Z|z)\\s*$", vals, perl = TRUE))
}

extract_datetime_raw_z_fraction <- function(lines, datetime_col = "Datetime", delim = ",") {
  if (length(lines) < 2) return(NULL)

  split_line <- function(line) parse_delimited_line(line, delim = delim)

  header <- split_line(lines[1])
  col_ix <- which(header == datetime_col)[1]
  if (is.na(col_ix)) return(NULL)

  body <- lines[-1]
  if (length(body) == 0) return(NULL)

  dt_vals <- vapply(body, function(line) {
    fields <- split_line(line)
    if (length(fields) < col_ix) return(NA_character_)
    val <- trimws(fields[col_ix])
    val <- sub('^"', '', val)
    sub('"$', '', val)
  }, character(1))

  dt_vals <- dt_vals[!is.na(dt_vals) & nzchar(dt_vals)]
  if (length(dt_vals) == 0) return(NULL)

  list(
    fraction = mean(grepl("(?:Z|z)\\s*$", dt_vals, perl = TRUE)),
    n_non_missing = length(dt_vals),
    col_ix = col_ix
  )
}

nice_n <- function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)

format_preview_column <- function(x) {
  if (inherits(x, "POSIXt")) {
    tz <- attr(x, "tzone")
    tz <- if (length(tz) == 0 || is.na(tz[1]) || !nzchar(tz[1])) "UTC" else tz[1]
    out <- format(x, tz = tz, usetz = TRUE)
    out[is.na(x)] <- NA_character_
    return(out)
  }

  x
}

build_data_preview <- function(data, n = 10L) {
  preview <- head(as.data.frame(data), as.integer(n))
  as.data.frame(lapply(preview, format_preview_column), stringsAsFactors = FALSE)
}

make_check <- function(level, id, title, status, message, details = NULL) {
  list(
    level = level,
    id = id,
    title = title,
    status = status,
    message = message,
    details = details
  )
}

status_icon <- function(status) {
  switch(
    status,
    pass = "✅",
    warn = "⚠️",
    fail = "❌",
    info = "ℹ️",
    "•"
  )
}

status_class <- function(status) {
  switch(
    status,
    pass = "success",
    warn = "warning",
    fail = "danger",
    info = "info",
    "secondary"
  )
}

validate_level_1_file <- function(path) {
  checks <- list()
  info <- file.info(path)

  if (is.na(info$size) || info$size <= 0) {
    checks[[length(checks) + 1]] <- make_check(
      1, "file_exists", "Readable file",
      "fail",
      "The uploaded file is empty or inaccessible."
    )
    return(checks)
  }

  checks[[length(checks) + 1]] <- make_check(
    1, "file_exists", "Readable file",
    "pass",
    paste0("The file is accessible and has size ", nice_n(info$size), " bytes.")
  )

  ext <- tolower(file_ext(path))
  if (ext %in% c("csv", "gz", "txt")) {
    checks[[length(checks) + 1]] <- make_check(
      1, "extension", "Filename extension",
      "pass",
      paste0("The file extension is .", ext, ".")
    )
  } else {
    checks[[length(checks) + 1]] <- make_check(
      1, "extension", "Filename extension",
      "warn",
      paste0("The file extension is .", ext, ". A .csv extension is recommended.")
    )
  }

  checks
}

validate_level_2_lines <- function(lines, path, skip = 0L, skip_reason = NULL) {
  checks <- list()

  if (length(lines) == 0) {
    checks[[length(checks) + 1]] <- make_check(
      2, "lines_present", "Text content available",
      "fail",
      "No text lines could be read from the file."
    )
    return(checks)
  }

  checks[[length(checks) + 1]] <- make_check(
    2, "lines_present", "Text content available",
    "pass",
    paste0("Successfully read ", nice_n(length(lines)), " initial lines for structural inspection.")
  )

  if (isTRUE(skip > 0)) {
    checks[[length(checks) + 1]] <- make_check(
      2, "header_skip", "Preamble/header offset",
      "info",
      paste0("Skipped ", skip, " line(s) before header detection and structure checks."),
      details = skip_reason
    )
  }

  header <- lines[1]
  guessed_sep <- guess_separator_from_header(header)
  guessed_label <- if (guessed_sep == "\t") "tab" else guessed_sep

  if (guessed_sep == ",") {
    checks[[length(checks) + 1]] <- make_check(
      2, "separator_guess", "Separator heuristic",
      "pass",
      "The header appears to use a comma separator."
    )
  } else {
    checks[[length(checks) + 1]] <- make_check(
      2, "separator_guess", "Separator heuristic",
      "warn",
      paste0("The header appears more consistent with the separator '", guessed_label, "' than with ','.")
    )
  }

  field_counts_csv <- vapply(lines, function(z) length(strsplit(z, ",", fixed = TRUE)[[1]]), integer(1))
  unique_counts <- sort(unique(field_counts_csv))

  if (length(unique_counts) == 1) {
    if (unique_counts == 1) {
      checks[[length(checks) + 1]] <- make_check(
        2, "single_column_csv", "Column count under comma parsing",
        "warn",
        "Parsing the inspected lines with ',' yields only one column. This often indicates that the wrong separator was used."
      )
    } else {
      checks[[length(checks) + 1]] <- make_check(
        2, "rectangular_text", "Rectangular structure under comma parsing",
        "pass",
        paste0("All inspected lines have ", unique_counts, " fields when split on ','.")
      )
    }
  } else {
    details <- paste0("Observed field counts in inspected lines: ", paste(unique_counts, collapse = ", "))
    checks[[length(checks) + 1]] <- make_check(
      2, "rectangular_text", "Rectangular structure under comma parsing",
      "warn",
      "The inspected lines do not all have the same number of comma-separated fields. This may indicate a non-rectangular file, quoting problems, or the wrong separator.",
      details = details
    )
  }

  checks
}

validate_level_3_import <- function(path, delim = ",", skip = 0L) {
  checks <- list()
  dat <- NULL
  err <- NULL

  dat <- tryCatch(
    safe_read_vroom(path, delim = delim, skip = skip),
    error = function(e) {
      err <<- conditionMessage(e)
      NULL
    }
  )

  
  if (is.null(dat)) {
    checks[[length(checks) + 1]] <- make_check(
      3, "vroom_read", "Read with vroom",
      "fail",
      "The file could not be read with vroom using a comma separator.",
      details = err
    )
    return(list(checks = checks, data = NULL))
  }

  checks[[length(checks) + 1]] <- make_check(
    3, "vroom_read", "Read with vroom",
    "pass",
    paste0(
      "The file was read successfully with vroom",
      if (skip > 0) paste0(" after skipping ", skip, " preamble line(s)") else "",
      ". Detected ", ncol(dat), " columns and ", nice_n(nrow(dat)), " rows."
    )
  )

  cls <- vapply(dat, function(x) class(x)[1], character(1))
  detected <- paste(paste(names(cls), cls, sep = ": "), collapse = "; ")

  checks[[length(checks) + 1]] <- make_check(
    3, "detected_types", "Auto-detected column types",
    "info",
    "Detected primary column classes.",
    details = detected
  )

  list(checks = checks, data = dat)
}

validate_level_4_names <- function(dat) {
  checks <- list()
  nms <- names(dat)

  if (is_unique(nms)) {
    checks[[length(checks) + 1]] <- make_check(
      4, "unique_names", "Unique variable names",
      "pass",
      "All variable names are unique."
    )
  } else {
    dupes <- unique(nms[duplicated(nms)])
    checks[[length(checks) + 1]] <- make_check(
      4, "unique_names", "Unique variable names",
      "fail",
      "Duplicate variable names were found.",
      details = paste(dupes, collapse = ", ")
    )
  }

  syntactic <- vapply(nms, syntactic_name_ok, logical(1))
  if (all(syntactic)) {
    checks[[length(checks) + 1]] <- make_check(
      4, "syntactic_names", "Syntactic variable names",
      "pass",
      "All variable names are syntactic."
    )
  } else {
    bad <- nms[!syntactic]
    suggestions <- paste0(bad, " → ", make.names(bad))
    checks[[length(checks) + 1]] <- make_check(
      4, "syntactic_names", "Syntactic variable names",
      "warn",
      "Some variable names are not syntactic.",
      details = paste(suggestions, collapse = "; ")
    )
  }

  checks
}

validate_level_5_datetime <- function(dat) {
  checks <- list()
  classes <- vapply(dat, function(x) class(x)[1], character(1))

  posix_cols <- names(classes)[classes %in% c("POSIXct", "POSIXlt")]
  if (length(posix_cols) > 0) {
    checks[[length(checks) + 1]] <- make_check(
      5, "posix_detected", "Datetime column detected",
      "pass",
      paste0("Detected POSIX datetime column(s): ", paste(posix_cols, collapse = ", "), ".")
    )
  } else {
    char_cols <- names(classes)[classes %in% c("character", "vroom_chr")]
    parsed_hits <- character(0)
    if (length(char_cols) > 0) {
      for (nm in char_cols) {
        vals <- dat[[nm]]
        vals <- vals[!is.na(vals) & nzchar(vals)]
        vals <- head(vals, 2000L)
        if (length(vals) == 0) next
        parsed <- parse_posix_utc(vals)
        frac <- mean(!is.na(parsed))
        if (isTRUE(frac > 0.9)) parsed_hits <- c(parsed_hits, nm)
      }
    }

    if (length(parsed_hits) > 0) {
      checks[[length(checks) + 1]] <- make_check(
        5, "posix_detected", "Datetime-like column detected",
        "warn",
        paste0("No POSIXct column was auto-detected, but these columns look parseable as UTC datetimes: ", paste(parsed_hits, collapse = ", "), ".")
      )
    } else {
      checks[[length(checks) + 1]] <- make_check(
        5, "posix_detected", "Datetime column detected",
        "fail",
        "No POSIXct column was auto-detected and no clearly parseable datetime-like column was found."
      )
    }
  }

  if ("Datetime" %in% names(dat)) {
    checks[[length(checks) + 1]] <- make_check(
      5, "datetime_name", "Suggested timestamp name",
      "pass",
      "A column named 'Datetime' is present."
    )
  } else {
    possible <- grep("date|time|datetime|timestamp", names(dat), ignore.case = TRUE, value = TRUE)
    details <- if (length(possible)) paste(possible, collapse = ", ") else NULL
    checks[[length(checks) + 1]] <- make_check(
      5, "datetime_name", "Suggested timestamp name",
      "warn",
      "No column named 'Datetime' was found. This is recommended for interoperability.",
      details = details
    )
  }

  checks
}

validate_level_6_temporal <- function(dat) {
  checks <- list()
  dt_col <- NULL

  if ("Datetime" %in% names(dat)) {
    dt_col <- "Datetime"
  } else {
    posix_cols <- names(dat)[vapply(dat, function(x) inherits(x, "POSIXt"), logical(1))]
    if (length(posix_cols) > 0) dt_col <- posix_cols[1]
  }

  if (is.null(dt_col)) {
    checks[[length(checks) + 1]] <- make_check(
      6, "temporal_checks", "Temporal validation",
      "info",
      "Temporal validation was skipped because no timestamp column could be identified."
    )
    return(checks)
  }

  x <- dat[[dt_col]]
  if (!inherits(x, "POSIXt")) {
    parsed <- parse_posix_utc(as.character(x))
    if (sum(!is.na(parsed)) == 0) {
      checks[[length(checks) + 1]] <- make_check(
        6, "temporal_checks", "Temporal validation",
        "fail",
        paste0("The candidate timestamp column '", dt_col, "' could not be parsed as UTC datetimes.")
      )
      return(checks)
    }
    x <- parsed
  }

  non_missing <- !is.na(x)
  x2 <- x[non_missing]

  if (length(x2) < 2) {
    checks[[length(checks) + 1]] <- make_check(
      6, "enough_timestamps", "Sufficient timestamps",
      "warn",
      "Fewer than two valid timestamps are available, so spacing cannot be assessed."
    )
    return(checks)
  }

  is_sorted <- !is.unsorted(x2, strictly = FALSE)
  checks[[length(checks) + 1]] <- make_check(
    6, "sorted_time", "Ascending timestamp order",
    if (is_sorted) "pass" else "warn",
    if (is_sorted) "Timestamps are in ascending order." else "Timestamps are not in ascending order."
  )

  any_dupes <- any(duplicated(x2))
  checks[[length(checks) + 1]] <- make_check(
    6, "unique_time", "Unique timestamps",
    if (!any_dupes) "pass" else "warn",
    if (!any_dupes) "All non-missing timestamps are unique." else "Duplicate timestamps were found."
  )

  dx <- as.numeric(diff(x2), units = "secs")
  dx <- dx[!is.na(dx)]
  if (length(dx) == 0) {
    checks[[length(checks) + 1]] <- make_check(
      6, "spacing_regular", "Regular observation spacing",
      "warn",
      "Timestamp intervals could not be assessed."
    )
    return(checks)
  }

  tab <- sort(table(dx), decreasing = TRUE)
  modal_step <- as.numeric(names(tab)[1])
  regular <- length(unique(dx)) == 1

  msg <- if (regular) {
    paste0("Observation spacing is regular at ", modal_step, " second(s).")
  } else {
    paste0("Observation spacing is not fully regular. Most common interval: ", modal_step, " second(s).")
  }

  details <- paste0("Unique interval counts (seconds): ", paste(paste(names(tab), tab, sep = "→"), collapse = ", "))

  checks[[length(checks) + 1]] <- make_check(
    6, "spacing_regular", "Regular observation spacing",
    if (regular) "pass" else "warn",
    msg,
    details = details
  )

  checks
}

validate_level_7_content <- function(dat, lines = NULL, lines_is_sample = FALSE) {
  checks <- list()

  if (ncol(dat) == 1) {
    checks[[length(checks) + 1]] <- make_check(
      7, "single_column_import", "Single imported column",
      "fail",
      "The file imported as a single column. This strongly suggests an incorrect separator or malformed structure."
    )
  } else {
    checks[[length(checks) + 1]] <- make_check(
      7, "single_column_import", "Single imported column",
      "pass",
      paste0("The file imported as ", ncol(dat), " columns.")
    )
  }

  if ("Datetime" %in% names(dat)) {
    raw_z <- extract_datetime_raw_z_fraction(lines)

    if (!is.null(raw_z) && !isTRUE(lines_is_sample)) {
      checks[[length(checks) + 1]] <- make_check(
        7, "utc_suffix", "UTC Z suffix in Datetime",
        if (isTRUE(raw_z$fraction > 0.95)) "pass" else "warn",
        if (isTRUE(raw_z$fraction > 0.95)) {
          "Most Datetime values end with 'Z' in the raw file, consistent with UTC representation."
        } else {
          "Many Datetime values do not end with 'Z' in the raw file. UTC with explicit 'Z' is recommended."
        },
        details = paste0(
          "Checked ", nice_n(raw_z$n_non_missing),
          " non-missing raw Datetime values in column ", raw_z$col_ix, "."
        )
      )
    } else if (!is.null(raw_z) && isTRUE(lines_is_sample)) {
      checks[[length(checks) + 1]] <- make_check(
        7, "utc_suffix", "UTC Z suffix in Datetime",
        "info",
        "UTC suffix estimation from raw lines is preview-only because the file is longer than the inspected slice.",
        details = paste0(
          "Preview estimate from ", nice_n(raw_z$n_non_missing),
          " non-missing Datetime values: ", round(raw_z$fraction * 100, 1), "% with trailing Z/z."
        )
      )
    } else {
      datetime_values <- dat$Datetime
      z_frac <- fraction_has_utc_z(datetime_values)
      if (!is.na(z_frac)) {
        checks[[length(checks) + 1]] <- make_check(
          7, "utc_suffix", "UTC Z suffix in Datetime",
          if (isTRUE(z_frac > 0.95)) "pass" else "warn",
          if (isTRUE(z_frac > 0.95)) {
            "Most Datetime values end with 'Z', consistent with UTC representation."
          } else {
            "Many Datetime values do not end with 'Z'. UTC with explicit 'Z' is recommended."
          },
          details = "Raw Datetime extraction was unavailable; suffix check was performed on imported Datetime values."
        )
      }
    }
  }

  checks
}

run_all_validations <- function(path, skip_lines = NULL, auto_detect_skip = TRUE) {
  raw_lines_preview <- safe_read_lines(path, n = 5001L)
  lines_is_sample <- length(raw_lines_preview) > 5000L
  raw_lines <- if (lines_is_sample) raw_lines_preview[seq_len(5000L)] else raw_lines_preview

  can_auto_detect <- isTRUE(auto_detect_skip) && !identical(tolower(file_ext(path)), "gz")
  detected <- if (can_auto_detect) detect_header_skip(raw_lines) else list(
    skip = 0L,
    header_line = if (length(raw_lines) > 0) raw_lines[1] else NA_character_,
    reason = if (identical(tolower(file_ext(path)), "gz")) {
      "Auto-detection was disabled for compressed input; defaulted to first line unless manual skip is provided."
    } else {
      "Auto-detection disabled; defaulted to first line unless manual skip is provided."
    }
  )
  effective_skip <- if (isTRUE(auto_detect_skip) && is.null(skip_lines)) {
    detected$skip
  } else {
    max(0L, as.integer(skip_lines %||% 0L))
  }

  lines <- skip_preamble_lines(raw_lines, effective_skip)

  out <- list()
  out$level_1 <- validate_level_1_file(path)
  out$level_2 <- validate_level_2_lines(lines, path, skip = effective_skip, skip_reason = detected$reason)

  imported <- validate_level_3_import(path, delim = ",", skip = effective_skip)
  out$level_3 <- imported$checks
  dat <- imported$data

  if (!is.null(dat)) {
    out$level_4 <- validate_level_4_names(dat)
    out$level_5 <- validate_level_5_datetime(dat)
    out$level_6 <- validate_level_6_temporal(dat)
    out$level_7 <- validate_level_7_content(dat, lines = lines, lines_is_sample = lines_is_sample)
  } else {
    out$level_4 <- list()
    out$level_5 <- list()
    out$level_6 <- list()
    out$level_7 <- list()
  }

  out$data <- dat
  out$lines <- lines
  out$raw_lines <- raw_lines
  out$skip_lines <- effective_skip
  out$lines_is_sample <- lines_is_sample
  out
}

flatten_checks <- function(results) {
  lvls <- grep("^level_", names(results), value = TRUE)
  unlist(results[lvls], recursive = FALSE)
}

check_card_ui <- function(chk) {
  div(
    class = paste("card border-", status_class(chk$status), " mb-3"),
    div(
      class = paste("card-header bg-", status_class(chk$status), " text-white"),
      tags$strong(paste(status_icon(chk$status), chk$title))
    ),
    div(
      class = "card-body",
      tags$p(class = "card-text", chk$message),
      if (!is.null(chk$details)) {
        tags$details(
          class = "details-toggle",
          tags$summary("Details"),
          tags$pre(style = "white-space: pre-wrap;", chk$details)
        )
      }
    )
  )
}

worst_status <- function(checks) {
  if (length(checks) == 0) return("info")
  statuses <- vapply(checks, `[[`, character(1), "status")
  if ("fail" %in% statuses) return("fail")
  if ("warn" %in% statuses) return("warn")
  if ("pass" %in% statuses) return("pass")
  "info"
}

level_tab_title <- function(stage_title, checks) {
  status <- worst_status(checks)
  count <- length(checks)
  paste0(status_icon(status), " ", stage_title, " (", count, ")")
}

level_panel_ui <- function(level_title, checks) {
  tagList(
    tags$h4(level_title),
    if (length(checks) == 0) {
      div(class = "alert alert-light", "No checks available at this stage.")
    } else {
      lapply(checks, check_card_ui)
    }
  )
}

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body { padding-bottom: 40px; }
      .main-title { font-weight: 700; margin-bottom: 0.8rem; }
      .sidebar-note { font-size: 0.95rem; color: #555; }
      .preview-box {
        border: 1px solid #d9e0ea;
        border-radius: 8px;
        padding: 12px;
        background: #f8fbff;
        max-height: 340px;
        overflow-y: auto;
        font-family: monospace;
        white-space: pre-wrap;
      }
      .card {
        border-radius: 10px;
        box-shadow: 0 1px 3px rgba(12, 28, 48, 0.08);
      }
      .details-toggle {
        margin-top: 0.5rem;
        border: 1px dashed #b5c2d3;
        border-radius: 8px;
        padding: 0.5rem 0.65rem;
        background: #f8fbff;
      }
      .details-toggle > summary {
        font-weight: 700;
        color: #0d6efd;
        cursor: pointer;
      }
      .details-toggle > summary:hover {
        text-decoration: underline;
      }
    "))
  ),
  tags$h2(class = "main-title", "Wearable CSV Validator"),
  fluidRow(
    column(
      width = 3,
      fileInput(
        "file",
        "Upload a wearable CSV file",
        accept = c(".csv", ".txt", ".gz")
      ),
      checkboxInput("show_preview", "Show raw line preview", value = TRUE),
      numericInput("preview_n", "Preview lines", value = 20, min = 5, max = 100, step = 5),
      checkboxInput("auto_skip_preamble", "Auto-detect and skip preamble lines", value = TRUE),
      numericInput("manual_skip_lines", "Manual lines to skip before header", value = 0, min = 0, step = 1),
      tags$p(
        class = "sidebar-note",
        "The validator performs staged checks: file access, text structure, import, variable names, datetime handling, and time-series regularity."
      ),
      hr(),
      uiOutput("summary_box")
    ),
    column(
      width = 9,
      uiOutput("results_tabs")
    )
  )
)

server <- function(input, output, session) {
  validation_results <- reactive({
    req(input$file)
    run_all_validations(
      input$file$datapath,
      skip_lines = if (isTRUE(input$auto_skip_preamble)) NULL else input$manual_skip_lines,
      auto_detect_skip = isTRUE(input$auto_skip_preamble)
    )
  })

  all_checks <- reactive({
    flatten_checks(validation_results())
  })

  output$summary_box <- renderUI({
    req(all_checks())
    chks <- all_checks()
    statuses <- vapply(chks, `[[`, character(1), "status")
    counts <- table(factor(statuses, levels = c("pass", "warn", "fail", "info")))

    div(
      class = "card",
      div(class = "card-header", tags$strong("Validation summary")),
      div(
        class = "card-body",
        tags$p(paste("Pass:", counts["pass"] %||% 0)),
        tags$p(paste("Warnings:", counts["warn"] %||% 0)),
        tags$p(paste("Failures:", counts["fail"] %||% 0)),
        tags$p(paste("Info:", counts["info"] %||% 0))
      )
    )
  })

  output$results_tabs <- renderUI({
    base_tabs <- list(
      tabPanel("Data preview", tableOutput("data_preview")),
      tabPanel("Raw lines", uiOutput("raw_preview"))
    )

    if (is.null(input$file)) {
      return(do.call(tabsetPanel, c(list(id = "validation_tabs"), base_tabs)))
    }

    results <- validation_results()
    do.call(tabsetPanel, c(
      list(
        id = "validation_tabs",
        tabPanel(level_tab_title("Stage 1: File", results$level_1), uiOutput("level_1_ui")),
        tabPanel(level_tab_title("Stage 2: Structure", results$level_2), uiOutput("level_2_ui")),
        tabPanel(level_tab_title("Stage 3: Import", results$level_3), uiOutput("level_3_ui")),
        tabPanel(level_tab_title("Stage 4: Names", results$level_4), uiOutput("level_4_ui")),
        tabPanel(level_tab_title("Stage 5: Datetime", results$level_5), uiOutput("level_5_ui")),
        tabPanel(level_tab_title("Stage 6: Time series", results$level_6), uiOutput("level_6_ui")),
        tabPanel(level_tab_title("Stage 7: Content", results$level_7), uiOutput("level_7_ui"))
      ),
      base_tabs
    ))
  })

  output$level_1_ui <- renderUI({
    req(validation_results())
    level_panel_ui("Stage 1: Basic file checks", validation_results()$level_1)
  })

  output$level_2_ui <- renderUI({
    req(validation_results())
    level_panel_ui("Stage 2: Text structure from readLines()", validation_results()$level_2)
  })

  output$level_3_ui <- renderUI({
    req(validation_results())
    level_panel_ui("Stage 3: Import with vroom()", validation_results()$level_3)
  })

  output$level_4_ui <- renderUI({
    req(validation_results())
    level_panel_ui("Stage 4: Variable names", validation_results()$level_4)
  })

  output$level_5_ui <- renderUI({
    req(validation_results())
    level_panel_ui("Stage 5: Datetime detection", validation_results()$level_5)
  })

  output$level_6_ui <- renderUI({
    req(validation_results())
    level_panel_ui("Stage 6: Temporal regularity", validation_results()$level_6)
  })

  output$level_7_ui <- renderUI({
    req(validation_results())
    level_panel_ui("Stage 7: Content conventions", validation_results()$level_7)
  })

  output$data_preview <- renderTable({
    req(validation_results()$data)
    build_data_preview(validation_results()$data, n = 10)
  }, striped = TRUE, bordered = TRUE, hover = TRUE)

  output$raw_preview <- renderUI({
    req(validation_results())
    if (!isTRUE(input$show_preview)) {
      return(div(class = "alert alert-light", "Raw preview is disabled."))
    }
    lines <- head(validation_results()$raw_lines, input$preview_n)
    div(class = "preview-box", HTML(paste(escape_html(lines), collapse = "\n")))
  })
}

shinyApp(ui, server)
