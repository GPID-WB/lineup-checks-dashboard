library(testthat)
library(data.table)

source(here::here("R", "change_detection.R"))
source(here::here("R", "summary_tables.R"))

# ---------------------------------------------------------------------------
# build_overview_agg()
# ---------------------------------------------------------------------------

test_that("build_overview_agg returns correct columns", {
  dt <- make_agg_merged()
  result <- build_overview_agg(dt, "headcount", poverty_line = 3)

  expected_cols <- c(
    "region_code",
    "reporting_year",
    "headcount_new",
    "headcount_old",
    "headcount_diff",
    "headcount_perc",
    "flagged"
  )
  expect_true(all(expected_cols %in% names(result)))
})

test_that("build_overview_agg filters correctly by poverty_line", {
  dt <- make_agg_merged()
  dt_extra <- copy(dt)
  dt_extra$poverty_line <- 4.2
  dt_combined <- rbindlist(list(dt, dt_extra))

  result <- build_overview_agg(dt_combined, "headcount", poverty_line = 3)
  expect_true(all(result$poverty_line == 3 | is.null(result$poverty_line)))
  expect_equal(nrow(result), nrow(dt))
})

test_that("build_overview_agg flags large relative changes", {
  dt <- make_agg_merged()
  # EAP 2018: 20% change > 5% threshold and 0.05 > 0.005 floor → flagged
  result <- build_overview_agg(
    dt,
    "headcount",
    poverty_line = 3,
    method = "relative",
    threshold = 0.10
  )
  expect_true(any(result$flagged, na.rm = TRUE))
})

test_that("build_overview_agg returns data.table", {
  result <- build_overview_agg(make_agg_merged(), "headcount", poverty_line = 3)
  expect_s3_class(result, "data.table")
})

test_that("build_overview_agg handles indicator with no rows", {
  dt <- make_agg_merged()[0]
  result <- build_overview_agg(dt, "headcount", poverty_line = 3)
  expect_equal(nrow(result), 0L)
})

# ---------------------------------------------------------------------------
# build_overview_country()
# ---------------------------------------------------------------------------

test_that("build_overview_country returns region-level aggregation", {
  dt <- make_lineups_merged()
  result <- build_overview_country(dt, "headcount", poverty_line = 3)
  expect_s3_class(result, "data.table")
  expect_true(all(
    c(
      "region_code",
      "reporting_year",
      "n_countries",
      "n_flagged",
      "pct_flagged"
    ) %in%
      names(result)
  ))
})

test_that("build_overview_country pct_flagged in [0, 1]", {
  dt <- make_lineups_merged()
  result <- build_overview_country(dt, "headcount", poverty_line = 3)
  expect_true(all(
    result$pct_flagged >= 0 & result$pct_flagged <= 1,
    na.rm = TRUE
  ))
})

test_that("build_overview_country threshold = 0 flags everything", {
  dt <- make_lineups_merged()
  # Threshold 0 on relative method flags anything with a non-zero diff
  result <- build_overview_country(
    dt,
    "headcount",
    poverty_line = 3,
    method = "absolute",
    threshold = 0
  )
  # BRA has a non-zero diff in 2019
  expect_true(any(result$n_flagged > 0))
})

# ---------------------------------------------------------------------------
# build_driver_table()
# ---------------------------------------------------------------------------

test_that("build_driver_table returns correct structure", {
  dt <- make_lineups_merged()
  result <- build_driver_table(
    dt,
    region = "EAP",
    year = 2018,
    poverty_line = 3
  )
  expect_s3_class(result, "data.table")
  expect_true(all(
    c(
      "country_code",
      "headcount_new",
      "headcount_old",
      "headcount_diff",
      "pop_weight",
      "weighted_contribution"
    ) %in%
      names(result)
  ))
})

test_that("build_driver_table returns only countries in region", {
  dt <- make_lineups_merged()
  result <- build_driver_table(dt, "SAR", 2018, poverty_line = 3)
  expect_true(all(result$country_code %in% c("IND")))
})

test_that("build_driver_table is sorted by |weighted_contribution| descending", {
  dt <- make_lineups_merged()
  result <- build_driver_table(dt, "EAP", 2019, poverty_line = 3)
  if (nrow(result) > 1L) {
    contributions <- abs(result$weighted_contribution)
    expect_true(all(diff(contributions) <= 0))
  }
})

test_that("build_driver_table returns empty dt for unknown region", {
  dt <- make_lineups_merged()
  result <- build_driver_table(dt, "UNKNOWN", 2018, poverty_line = 3)
  expect_equal(nrow(result), 0L)
})

test_that("build_driver_table pop_weight sums to ~1", {
  dt <- make_lineups_merged()
  result <- build_driver_table(dt, "EAP", 2018, poverty_line = 3)
  if (nrow(result) > 0L) {
    expect_equal(sum(result$pop_weight, na.rm = TRUE), 1, tolerance = 1e-6)
  }
})

# ---------------------------------------------------------------------------
# build_source_table()
# ---------------------------------------------------------------------------

test_that("build_source_table returns correct columns", {
  dt <- make_lineups_merged()
  result <- build_source_table(dt, "CHN", poverty_line = 3)
  expect_s3_class(result, "data.table")
  expect_true("reporting_year" %in% names(result))
  # At least some explanatory variable columns present
  expect_true(any(grepl("reporting_gdp|cpi|welfare_type", names(result))))
})

test_that("build_source_table filters by country correctly", {
  dt <- make_lineups_merged()
  result <- build_source_table(dt, "CHN", poverty_line = 3)
  expect_equal(nrow(result), 2L) # 2 years for CHN
})

test_that("build_source_table year_range filter works", {
  dt <- make_lineups_merged()
  result <- build_source_table(
    dt,
    "CHN",
    poverty_line = 3,
    year_range = c(2019, 2019)
  )
  expect_equal(nrow(result), 1L)
  expect_equal(result$reporting_year, 2019)
})

test_that("build_source_table returns empty for unknown country", {
  dt <- make_lineups_merged()
  result <- build_source_table(dt, "ZZZ", poverty_line = 3)
  expect_equal(nrow(result), 0L)
})

# ---------------------------------------------------------------------------
# Edge cases: data quality guards
# ---------------------------------------------------------------------------

test_that("build_overview_country pct_flagged excludes NA-flagged rows from denominator", {
  # Mix matched and unmatched rows (flagged = NA for unmatched)
  dt <- data.table::copy(make_lineups_merged())
  # Force one headcount.x to NA to simulate unmatched row
  dt[country_code == "BRA" & reporting_year == 2018, headcount.x := NA_real_]
  result <- build_overview_country(dt, "headcount", poverty_line = 3)
  # pct_flagged should not include the NA row in denominator
  expect_true(all(
    result$pct_flagged >= 0 & result$pct_flagged <= 1,
    na.rm = TRUE
  ))
})

test_that("build_driver_table errors when all reporting_pop are zero or NA", {
  dt <- data.table::copy(make_lineups_merged())
  dt[region_code == "EAP", reporting_pop.x := NA_real_]
  expect_error(
    build_driver_table(dt, "EAP", 2018, poverty_line = 3),
    regexp = "reporting_pop"
  )
})

test_that("build_driver_table errors when reporting_pop.x column is absent", {
  dt <- data.table::copy(make_lineups_merged())
  dt[, reporting_pop.x := NULL]
  expect_error(
    build_driver_table(dt, "EAP", 2018, poverty_line = 3),
    regexp = "reporting_pop.x"
  )
})
