library(testthat)
library(data.table)

source(here::here("R", "process_data.R"))

# ---------------------------------------------------------------------------
# harmonize_old_cols()
# ---------------------------------------------------------------------------

test_that("harmonize_old_cols renames gdp, hfce, pop correctly", {
  dt <- data.table(
    gdp = 100,
    hfce = 50,
    pop = 1e6,
    headcount = 0.3
  )
  result <- harmonize_old_cols(dt)
  expect_true("reporting_gdp" %in% names(result))
  expect_true("reporting_pce" %in% names(result))
  expect_true("reporting_pop" %in% names(result))
  expect_false("gdp" %in% names(result))
  expect_false("hfce" %in% names(result))
  expect_false("pop" %in% names(result))
  # Other columns preserved
  expect_true("headcount" %in% names(result))
})

test_that("harmonize_old_cols is a no-op when legacy cols are absent", {
  dt <- data.table(reporting_gdp = 100, headcount = 0.3)
  before <- copy(dt)
  result <- harmonize_old_cols(dt)
  expect_identical(names(result), names(before))
})

test_that("harmonize_old_cols handles partial overlap", {
  dt <- data.table(gdp = 5, headcount = 0.1)
  harmonize_old_cols(dt)
  expect_true("reporting_gdp" %in% names(dt))
  expect_false("gdp" %in% names(dt))
  # hfce and pop were absent — no error
  expect_false("reporting_pce" %in% names(dt))
})

# ---------------------------------------------------------------------------
# add_diff_cols()
# ---------------------------------------------------------------------------

test_that("add_diff_cols computes correct absolute, relative and ratio cols", {
  dt <- data.table(mean.x = 12, mean.y = 10)
  add_diff_cols(dt, "mean")
  expect_equal(dt$mean_diff, 2)
  expect_equal(dt$mean_perc, 0.2)
  expect_equal(dt$mean_ratio, 1.2)
})

test_that("add_diff_cols guards against division by zero with epsilon", {
  dt <- data.table(mean.x = 1, mean.y = 0)
  expect_no_error(add_diff_cols(dt, "mean"))
  expect_true(is.finite(dt$mean_perc))
  expect_true(is.finite(dt$mean_ratio))
})

test_that("add_diff_cols handles negative old values correctly", {
  # negative old values: |old| is used in denominator
  dt <- data.table(mean.x = -1, mean.y = -2)
  add_diff_cols(dt, "mean")
  expect_equal(dt$mean_diff, 1)
  # perc = 1 / 2 = 0.5
  expect_equal(dt$mean_perc, 0.5)
})

test_that("add_diff_cols handles NA values without error", {
  dt <- data.table(mean.x = NA_real_, mean.y = 10)
  expect_no_error(add_diff_cols(dt, "mean"))
  expect_true(is.na(dt$mean_diff))
})

test_that("add_diff_cols works for multiple variables", {
  dt <- data.table(
    headcount.x = 0.3,
    headcount.y = 0.25,
    mean.x = 15,
    mean.y = 10
  )
  add_diff_cols(dt, c("headcount", "mean"))
  expect_true(all(
    c(
      "headcount_diff",
      "mean_diff",
      "headcount_perc",
      "mean_perc",
      "headcount_ratio",
      "mean_ratio"
    ) %in%
      names(dt)
  ))
})

# ---------------------------------------------------------------------------
# add_categorical_change_cols()
# ---------------------------------------------------------------------------

test_that("add_categorical_change_cols flags differences correctly", {
  dt <- data.table(
    welfare_type.x = c("consumption", "income", NA),
    welfare_type.y = c("consumption", "consumption", NA)
  )
  add_categorical_change_cols(dt, "welfare_type")
  expect_equal(dt$welfare_type_changed, c(FALSE, TRUE, FALSE))
})

test_that("add_categorical_change_cols flags one-sided NA as changed", {
  dt <- data.table(
    welfare_type.x = c("consumption", NA),
    welfare_type.y = c(NA, "income")
  )
  add_categorical_change_cols(dt, "welfare_type")
  expect_equal(dt$welfare_type_changed, c(TRUE, TRUE))
})

# ---------------------------------------------------------------------------
# detect_survey_coverage_changes()
# ---------------------------------------------------------------------------

make_survey <- function(country_codes, years) {
  data.table(
    country_code = rep(country_codes, each = length(years)),
    reporting_year = rep(years, times = length(country_codes)),
    reporting_level = "national",
    welfare_type = "consumption"
  )
}

test_that("detect_survey_coverage_changes returns added and dropped", {
  new_dt <- make_survey(c("ARG", "BRA", "CHL"), 2010:2012)
  old_dt <- make_survey(c("ARG", "BRA"), 2010:2012)
  result <- detect_survey_coverage_changes(new_dt, old_dt)
  expect_true(nrow(result[change == "added"]) > 0L)
  expect_true(nrow(result[change == "dropped"]) == 0L)
  expect_true("CHL" %in% result[change == "added"]$country_code)
})

test_that("detect_survey_coverage_changes returns empty when no changes", {
  survey <- make_survey("ARG", 2010)
  result <- detect_survey_coverage_changes(survey, survey)
  expect_equal(nrow(result), 0L)
})

test_that("detect_survey_coverage_changes detects dropped surveys", {
  new_dt <- make_survey("ARG", 2010)
  old_dt <- make_survey(c("ARG", "BRA"), 2010)
  result <- detect_survey_coverage_changes(new_dt, old_dt)
  expect_true("BRA" %in% result[change == "dropped"]$country_code)
})

# ---------------------------------------------------------------------------
# process_ppp_data_extended() — integration test using real .fst files
# Skipped if data files are not available (e.g., CI environments).
# ---------------------------------------------------------------------------

skip_if_no_data <- function(ppp_year) {
  path <- here::here("data", ppp_year, "lyears.fst")
  skip_if_not(file.exists(path), paste("Data not available:", path))
}

test_that("process_ppp_data_extended returns correct list structure for 2021", {
  skip_if_no_data("2021")
  d <- process_ppp_data_extended("2021", data_dir = here::here("data"))

  expect_type(d, "list")
  expected_keys <- c(
    "surveys_merged",
    "lineups_merged",
    "agg_merged",
    "survey_indicators",
    "lineup_indicators",
    "agg_indicators",
    "expl_vars",
    "cat_expl_vars",
    "countries_available",
    "regions_available",
    "dup_countries",
    "dt_survey_new",
    "survey_coverage_diff"
  )
  expect_true(all(expected_keys %in% names(d)))
})

test_that("process_ppp_data_extended merges carry diff columns for indicators", {
  skip_if_no_data("2021")
  d <- process_ppp_data_extended("2021", data_dir = here::here("data"))

  expect_true("headcount_diff" %in% names(d$surveys_merged))
  expect_true("headcount_perc" %in% names(d$surveys_merged))
  expect_true("headcount_ratio" %in% names(d$surveys_merged))
  expect_true("mean_diff" %in% names(d$lineups_merged))
})

test_that("process_ppp_data_extended carries explanatory variable diffs", {
  skip_if_no_data("2021")
  d <- process_ppp_data_extended("2021", data_dir = here::here("data"))

  # At least reporting_gdp and cpi should produce diff cols in lineup merge
  expl_diff_cols <- grep("_diff$", names(d$lineups_merged), value = TRUE)
  expect_true(any(grepl("reporting_gdp|cpi|reporting_pop", expl_diff_cols)))
})

test_that("process_ppp_data_extended survey_coverage_diff has expected columns", {
  skip_if_no_data("2021")
  d <- process_ppp_data_extended("2021", data_dir = here::here("data"))

  expect_true(
    all(
      c("country_code", "reporting_year", "change") %in%
        names(d$survey_coverage_diff)
    )
  )
  expect_true(all(d$survey_coverage_diff$change %in% c("added", "dropped")))
})
