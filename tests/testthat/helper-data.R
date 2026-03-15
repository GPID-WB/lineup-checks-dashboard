# ============================================================
# helper-data.R
# Shared synthetic data factories for all testthat test files.
# Sourced automatically by testthat before running tests.
# ============================================================

#' Build a minimal synthetic agg_merged data.table for tests
make_agg_merged <- function() {
  data.table::data.table(
    region_code    = c("EAP", "EAP", "SAR", "SAR", "WLD", "WLD"),
    reporting_year = c(2018, 2019, 2018, 2019, 2018, 2019),
    poverty_line   = 3,
    headcount.x    = c(0.30, 0.28, 0.50, 0.48, 0.40, 0.38),
    headcount.y    = c(0.25, 0.25, 0.45, 0.45, 0.36, 0.36),
    headcount_diff = c(0.05, 0.03, 0.05, 0.03, 0.04, 0.02),
    headcount_perc = c(0.20, 0.12, 0.11, 0.067, 0.11, 0.055),
    mean.x         = c(10, 11, 5, 5.5, 7.5, 8),
    mean.y         = c(10, 10, 5, 5, 7, 7.5),
    mean_diff      = c(0, 1, 0, 0.5, 0.5, 0.5),
    mean_perc      = c(0, 0.1, 0, 0.1, 0.07, 0.07),
    .joyn          = "x & y"
  )
}

#' Build a minimal synthetic lineups_merged data.table for tests
make_lineups_merged <- function() {
  data.table::data.table(
    country_code          = c("CHN", "CHN", "IND", "IND", "BRA", "BRA"),
    region_code           = c("EAP", "EAP", "SAR", "SAR", "LAC", "LAC"),
    country_name          = c("China", "China", "India", "India", "Brazil", "Brazil"),
    reporting_year        = c(2018, 2019, 2018, 2019, 2018, 2019),
    poverty_line          = 3,
    headcount.x           = c(0.20, 0.18, 0.40, 0.38, 0.10, 0.09),
    headcount.y           = c(0.15, 0.15, 0.36, 0.36, 0.10, 0.10),
    headcount_diff        = c(0.05, 0.03, 0.04, 0.02, 0.00, -0.01),
    headcount_perc        = c(0.33, 0.20, 0.11, 0.056, 0, -0.10),
    reporting_pop.x       = c(1.4e9, 1.4e9, 1.3e9, 1.3e9, 2e8, 2e8),
    reporting_gdp.x       = c(8000, 8500, 2000, 2100, 15000, 15500),
    reporting_gdp.y       = c(7500, 7500, 1900, 2000, 15000, 15000),
    reporting_gdp_diff    = c(500, 1000, 100, 100, 0, 500),
    cpi.x                 = c(1.0, 1.05, 1.0, 1.02, 1.0, 1.0),
    cpi.y                 = c(1.0, 1.00, 1.0, 1.00, 1.0, 1.0),
    cpi_diff              = c(0, 0.05, 0, 0.02, 0, 0),
    welfare_type.x        = c(
      "consumption", "consumption",
      "consumption", "consumption",
      "income",      "income"
    ),
    welfare_type.y        = c(
      "consumption", "consumption",
      "consumption", "income",
      "income",      "income"
    ),
    welfare_type_changed  = c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE),
    .joyn                 = "x & y"
  )
}

#' Build a minimal synthetic survey data.table for tests
#'
#' @param country_codes Character vector of country codes.
#' @param years Integer vector of reporting years.
make_survey <- function(country_codes, years) {
  data.table::data.table(
    country_code    = rep(country_codes, each = length(years)),
    reporting_year  = rep(years, times = length(country_codes)),
    reporting_level = "national",
    welfare_type    = "consumption"
  )
}
