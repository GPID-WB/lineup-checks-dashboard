# ============================================================
# test-country-detail-plot.R
# Tests for the Country Detail: Lineup Time Series plot data preparation.
# ============================================================

library(testthat)
library(data.table)

# ---------------------------------------------------------------------------
# Replication of the *fixed* data-prep logic from the plot_country_ts
# reactive in lineup-checks-revamp.qmd.
# reporting_level is preserved as an id.var so multi-level countries
# render as distinct series in one chart.
# ---------------------------------------------------------------------------
.prep_country_ts <- function(lineup_data, indicator) {
  ind_x <- paste0(indicator, ".x")
  ind_y <- paste0(indicator, ".y")

  plot_data <- lineup_data[, list(
    reporting_year,
    reporting_level,
    new_val = get(ind_x),
    old_val = get(ind_y)
  )]
  plot_data <- data.table::melt(
    plot_data,
    id.vars = c("reporting_year", "reporting_level"),
    variable.name = "version",
    value.name = "value"
  )
  plot_data[,
    version := data.table::fifelse(version == "new_val", "New", "Old")
  ]
  plot_data
}

# ---------------------------------------------------------------------------
# Helper: country with two reporting levels (national + urban)
# ---------------------------------------------------------------------------
make_multi_level_lineup <- function() {
  data.table::data.table(
    country_code = rep("CHN", 4),
    reporting_year = c(2018L, 2019L, 2018L, 2019L),
    reporting_level = c("national", "national", "urban", "urban"),
    welfare_type = "consumption",
    poverty_line = 3,
    headcount.x = c(0.20, 0.18, 0.30, 0.27),
    headcount.y = c(0.15, 0.15, 0.24, 0.24)
  )
}

# ---------------------------------------------------------------------------
# Tests for the fixed data-prep logic
# ---------------------------------------------------------------------------

test_that("plot_country_ts data includes reporting_level for multi-level countries", {
  lineup_data <- make_multi_level_lineup()

  plot_data <- .prep_country_ts(lineup_data, "headcount")

  expect_true(
    "reporting_level" %in% names(plot_data),
    info = paste(
      "reporting_level must be preserved so that countries with",
      "national/urban/rural levels render as separate series in one chart,",
      "not blended into a single undifferentiated line."
    )
  )
})

test_that("each reporting_level produces a distinct series in plot_data", {
  lineup_data <- make_multi_level_lineup()

  plot_data <- .prep_country_ts(lineup_data, "headcount")

  # 2 reporting levels × 2 versions = 4 distinct series identifiers
  n_series <- data.table::uniqueN(
    plot_data,
    by = c("reporting_level", "version")
  )
  expect_equal(n_series, 4L)
})

test_that("single-level country produces 2 series (New and Old only)", {
  lineup_data <- data.table::data.table(
    country_code = rep("BRA", 2),
    reporting_year = c(2018L, 2019L),
    reporting_level = "national",
    welfare_type = "income",
    poverty_line = 3,
    headcount.x = c(0.10, 0.09),
    headcount.y = c(0.10, 0.10)
  )

  plot_data <- .prep_country_ts(lineup_data, "headcount")

  expect_equal(data.table::uniqueN(plot_data$version), 2L)
  expect_equal(data.table::uniqueN(plot_data$reporting_level), 1L)
})

test_that("version column contains only 'New' and 'Old' after prep", {
  lineup_data <- make_multi_level_lineup()

  plot_data <- .prep_country_ts(lineup_data, "headcount")

  expect_setequal(unique(plot_data$version), c("New", "Old"))
})
