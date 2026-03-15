library(testthat)
library(data.table)

source(here::here("R", "change_detection.R"))

# ---------------------------------------------------------------------------
# flag_relative()
# ---------------------------------------------------------------------------

test_that("flag_relative flags large relative changes above both thresholds", {
  # 20% change AND > 0.005 absolute change → flagged
  expect_true(flag_relative(
    new_val = 0.12,
    old_val = 0.10,
    rel_thresh = 0.05,
    abs_floor = 0.005
  ))
})

test_that("flag_relative does NOT flag when relative change is small", {
  # 2% change — below 5% threshold
  expect_false(flag_relative(
    new_val = 0.102,
    old_val = 0.10,
    rel_thresh = 0.05,
    abs_floor = 0.005
  ))
})

test_that("flag_relative suppresses tiny absolute changes (abs_floor)", {
  # 200% relative change, but |diff| = 0.0002 < abs_floor = 0.005
  expect_false(flag_relative(
    new_val = 0.0003,
    old_val = 0.0001,
    rel_thresh = 0.05,
    abs_floor = 0.005
  ))
})

test_that("flag_relative handles zero old values without error", {
  expect_no_error(flag_relative(new_val = 0.1, old_val = 0.0))
  result <- flag_relative(
    new_val = 0.1,
    old_val = 0.0,
    rel_thresh = 0.05,
    abs_floor = 0.005
  )
  # 0.1 absolute diff > 0.005 floor, and 0.1 / epsilon >> 0.05
  expect_true(result)
})

test_that("flag_relative returns NA when inputs are NA", {
  result <- flag_relative(NA_real_, 0.1)
  expect_true(is.na(result))
})

test_that("flag_relative works on vectors", {
  new_v <- c(0.12, 0.101, 0.0003, NA)
  old_v <- c(0.10, 0.100, 0.0001, 0.5)
  result <- flag_relative(new_v, old_v, rel_thresh = 0.05, abs_floor = 0.005)
  expect_equal(result, c(TRUE, FALSE, FALSE, NA))
})

# ---------------------------------------------------------------------------
# flag_absolute()
# ---------------------------------------------------------------------------

test_that("flag_absolute flags changes above threshold", {
  expect_true(flag_absolute(0.12, 0.10, abs_thresh = 0.01))
})

test_that("flag_absolute does NOT flag changes at or below threshold", {
  expect_false(flag_absolute(0.105, 0.10, abs_thresh = 0.01))
})

test_that("flag_absolute handles identical values", {
  expect_false(flag_absolute(0.10, 0.10))
})

test_that("flag_absolute returns NA for NA inputs", {
  expect_true(is.na(flag_absolute(NA_real_, 0.10)))
})

# ---------------------------------------------------------------------------
# flag_zscore()
# ---------------------------------------------------------------------------

test_that("flag_zscore flags extreme outliers", {
  set.seed(42)
  new_v <- rnorm(100, mean = 5, sd = 0.1)
  old_v <- rnorm(100, mean = 5, sd = 0.1)
  # Insert an obvious outlier
  new_v[1] <- 50
  result <- flag_zscore(new_v, old_v, z_thresh = 2.0)
  expect_true(result[1])
})

test_that("flag_zscore does not flag when all diffs are constant", {
  # All diffs identical → SD = 0 → no flags
  result <- flag_zscore(rep(1.1, 10), rep(1.0, 10), z_thresh = 2.0)
  expect_true(all(result == FALSE))
})

test_that("flag_zscore handles NA values gracefully", {
  new_v <- c(1, 2, NA, 4)
  old_v <- c(1, 1, 1, 1)
  expect_no_error(flag_zscore(new_v, old_v))
})

test_that("flag_zscore returns logical vector of correct length", {
  result <- flag_zscore(1:10, rep(5, 10))
  expect_type(result, "logical")
  expect_length(result, 10L)
})

# ---------------------------------------------------------------------------
# flag_changes() dispatcher
# ---------------------------------------------------------------------------

test_that("flag_changes dispatches to relative method correctly", {
  result <- flag_changes(0.12, 0.10, method = "relative", threshold = 0.05)
  expect_true(result)
})

test_that("flag_changes dispatches to absolute method correctly", {
  result <- flag_changes(0.12, 0.10, method = "absolute", threshold = 0.01)
  expect_true(result)
})

test_that("flag_changes dispatches to zscore method correctly", {
  set.seed(1)
  new_v <- c(rnorm(99, 5, 0.1), 50)
  old_v <- rep(5, 100)
  result <- flag_changes(new_v, old_v, method = "zscore", threshold = 2.0)
  expect_true(result[100])
})

test_that("flag_changes errors on unknown method", {
  expect_error(flag_changes(1, 1, method = "invalid"))
})

test_that("flag_changes uses default threshold when NULL", {
  # Should not error and should return a logical
  result <- flag_changes(0.12, 0.10, method = "relative", threshold = NULL)
  expect_type(result, "logical")
})

# ---------------------------------------------------------------------------
# add_flag_col()
# ---------------------------------------------------------------------------

test_that("add_flag_col adds correct _flagged column to data.table", {
  dt <- data.table(headcount.x = c(0.3, 0.1), headcount.y = c(0.25, 0.1))
  add_flag_col(dt, "headcount", method = "relative")
  expect_true("headcount_flagged" %in% names(dt))
  expect_type(dt$headcount_flagged, "logical")
})

test_that("add_flag_col modifies dt by reference", {
  dt <- data.table(headcount.x = 0.3, headcount.y = 0.1)
  dt_ref <- dt # reference, not copy
  add_flag_col(dt, "headcount")
  expect_true("headcount_flagged" %in% names(dt_ref))
})

# ---------------------------------------------------------------------------
# summarize_flags()
# ---------------------------------------------------------------------------

test_that("summarize_flags returns a data.table with correct columns", {
  dt <- data.table(
    headcount.x = c(0.3, 0.1, 0.5),
    headcount.y = c(0.25, 0.1, 0.1),
    mean.x = c(15, 10, 20),
    mean.y = c(10, 10, 10)
  )
  result <- summarize_flags(dt, c("headcount", "mean"))
  expect_s3_class(result, "data.table")
  expect_true(all(
    c("indicator", "n_flagged", "n_total", "pct_flagged") %in%
      names(result)
  ))
  expect_equal(nrow(result), 2L)
})

test_that("summarize_flags skips indicators not present in data", {
  dt <- data.table(headcount.x = 0.3, headcount.y = 0.2)
  result <- summarize_flags(dt, c("headcount", "gini"))
  expect_equal(result$indicator, "headcount")
})

test_that("summarize_flags pct_flagged is in [0, 1]", {
  dt <- data.table(
    mean.x = c(10, 11, 12),
    mean.y = c(10, 10, 10)
  )
  result <- summarize_flags(dt, "mean", method = "relative", threshold = 0.05)
  expect_true(all(
    result$pct_flagged >= 0 & result$pct_flagged <= 1,
    na.rm = TRUE
  ))
})
