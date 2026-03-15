library(testthat)
library(data.table)

source(here::here("R", "utils.R"))

# ---------------------------------------------------------------------------
# load_from_repo() — requires network access; skipped in CI
# ---------------------------------------------------------------------------

test_that("load_from_repo errors on invalid ppp_year argument", {
  expect_error(
    load_from_repo("aggregates.fst", ppp_year = "2000"),
    regexp = "arg"  # match.arg error message
  )
})

test_that("load_from_repo errors with informative message on HTTP failure", {
  skip_if_offline()
  # Use a valid year but a filename that doesn't exist on GitHub
  expect_error(
    load_from_repo("nonexistent_file.fst", ppp_year = "2021"),
    regexp = "404|download"  # httr::stop_for_status message
  )
})

# ---------------------------------------------------------------------------
# Integration: end-to-end download (only on machines with network + access)
# ---------------------------------------------------------------------------

test_that("load_from_repo returns a non-empty data.table", {
  skip_if_offline()
  skip_on_ci()
  dt <- load_from_repo("aggregates.fst", ppp_year = "2021")
  expect_s3_class(dt, "data.table")
  expect_gt(nrow(dt), 0L)
  expect_true("region_code" %in% names(dt))
})
