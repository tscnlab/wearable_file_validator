# Wearable CSV Validator

## Run

```r
shiny::runApp("app.R")
```

## Required packages

```r
install.packages(c("shiny", "vroom"))
```

## Design notes

The app is organized into staged validation levels:

1. Basic file checks
2. Text structure via `readLines()`
3. Import with `vroom()`
4. Variable name checks
5. Datetime detection
6. Temporal regularity
7. Content conventions

To extend the validator, add a new `validate_level_*()` function and register it in `run_all_validations()`.

## Efficiency

- `readLines()` is only used for a bounded initial structural scan.
- `vroom()` is used for efficient parsing of large files.
- Datetime probing is sampled where appropriate.
- Checks are modular so heavier validations can be added later without restructuring the app.
