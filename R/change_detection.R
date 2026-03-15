# ============================================================
# change_detection.R
# Functions for flagging significant differences between old
# and new pipeline estimates. Three methods are supported:
#   1. Relative difference with an absolute floor (default)
#   2. Absolute difference
#   3. Z-score of the distribution of differences
# ============================================================

# Default thresholds — used when callers do not supply values
DEFAULT_REL_THRESH <- 0.05 # 5% relative change
DEFAULT_ABS_FLOOR <- 0.005 # minimum absolute change required
DEFAULT_ABS_THRESH <- 0.01 # absolute threshold (method = "absolute")
DEFAULT_Z_THRESH <- 2.0 # z-score threshold (method = "zscore")


#' Flag rows by relative difference with an absolute floor
#'
#' A row is flagged when **both** conditions hold:
#' 1. `|new - old| / max(|old|, epsilon) > rel_thresh`
#' 2. `|new - old| > abs_floor`
#'
#' The absolute floor prevents spurious flags when `old` is very small
#' (e.g., a headcount of 0.001 changing to 0.002 is a 100% relative change
#' but only a 0.001 pp absolute change).
#'
#' @param new_val Numeric vector. New (pipeline) values.
#' @param old_val Numeric vector. Old (published) values.
#' @param rel_thresh Numeric scalar. Relative threshold (proportion, not %).
#'   Defaults to `0.05`.
#' @param abs_floor Numeric scalar. Minimum absolute change required for
#'   flagging. Defaults to `0.005`.
#' @param epsilon Numeric scalar. Denominator floor to prevent
#'   division by zero. Defaults to `1e-9`.
#'
#' @return Logical vector, same length as `new_val`. `NA` when either
#'   input is `NA`.
#'
#' @family change_detection
#' @export
#'
#' @examples
#' flag_relative(c(0.12, 10.5, 0.001), c(0.10, 10.0, 0.0009))
flag_relative <- function(
  new_val,
  old_val,
  rel_thresh = DEFAULT_REL_THRESH,
  abs_floor = DEFAULT_ABS_FLOOR,
  epsilon = 1e-9
) {
  stopifnot(
    is.numeric(new_val),
    is.numeric(old_val),
    length(new_val) == length(old_val)
  )
  abs_diff <- abs(new_val - old_val)
  rel_diff <- abs_diff / pmax(abs(old_val), epsilon)
  return(rel_diff > rel_thresh & abs_diff > abs_floor)
}


#' Flag rows by absolute difference
#'
#' A row is flagged when `|new - old| > abs_thresh`.
#'
#' @param new_val Numeric vector. New (pipeline) values.
#' @param old_val Numeric vector. Old (published) values.
#' @param abs_thresh Numeric scalar. Absolute threshold for flagging.
#'   Defaults to `0.01`.
#'
#' @return Logical vector, same length as `new_val`. `NA` when either
#'   input is `NA`.
#'
#' @family change_detection
#' @export
#'
#' @examples
#' flag_absolute(c(0.12, 10.5, 0.001), c(0.10, 10.0, 0.0009))
flag_absolute <- function(new_val, old_val, abs_thresh = DEFAULT_ABS_THRESH) {
  stopifnot(
    is.numeric(new_val),
    is.numeric(old_val),
    length(new_val) == length(old_val)
  )
  return(abs(new_val - old_val) > abs_thresh)
}


#' Flag rows by z-score of the difference distribution
#'
#' Computes `diff = new - old`, then standardises to `z = (diff - mean) / sd`.
#' A row is flagged when `|z| > z_thresh`. Mean and SD are computed after
#' trimming the top and bottom 1% of diffs to reduce sensitivity to extreme
#' outliers.
#'
#' @param new_val Numeric vector. New (pipeline) values.
#' @param old_val Numeric vector. Old (published) values.
#' @param z_thresh Numeric scalar. Z-score threshold for flagging.
#'   Defaults to `2.0`.
#' @param trim Numeric scalar in [0, 0.5]. Trimming fraction for mean/SD
#'   computation. Defaults to `0.01`.
#'
#' @return Logical vector, same length as `new_val`. `NA` when the SD is
#'   zero or when individual inputs are `NA`.
#'
#' @family change_detection
#' @export
#'
#' @examples
#' set.seed(42)
#' new_v <- rnorm(100, mean = 5, sd = 1)
#' old_v <- rnorm(100, mean = 5, sd = 1)
#' new_v[1] <- 20  # outlier
#' flag_zscore(new_v, old_v)
flag_zscore <- function(
  new_val,
  old_val,
  z_thresh = DEFAULT_Z_THRESH,
  trim = 0.01
) {
  stopifnot(
    is.numeric(new_val),
    is.numeric(old_val),
    length(new_val) == length(old_val)
  )
  diffs <- new_val - old_val
  # Compute both tail quantiles in a single pass
  qs <- quantile(diffs, c(trim, 1 - trim), na.rm = TRUE)
  trimmed <- diffs[diffs >= qs[[1L]] & diffs <= qs[[2L]]]

  mu <- mean(trimmed, na.rm = TRUE)
  sg <- sd(trimmed, na.rm = TRUE)

  # Guard near-zero SD: floating-point noise can produce tiny non-zero SDs
  if (is.na(sg) || sg < sqrt(.Machine$double.eps)) {
    # All values are effectively identical — no meaningful changes
    return(rep(FALSE, length(diffs)))
  }

  z <- (diffs - mu) / sg
  return(abs(z) > z_thresh)
}


#' Dispatcher: flag significant changes using the selected method
#'
#' Applies one of three significance-detection methods to a pair of
#' numeric vectors, returning a logical flag vector.
#'
#' @param new_val Numeric vector. New (pipeline) values.
#' @param old_val Numeric vector. Old (published) values.
#' @param method Character scalar. One of `"relative"` (default),
#'   `"absolute"`, or `"zscore"`.
#' @param threshold Numeric scalar. The primary threshold:
#'   - `"relative"`: relative threshold (proportion). Default: `0.05`.
#'   - `"absolute"`: absolute threshold. Default: `0.01`.
#'   - `"zscore"`: z-score threshold. Default: `2.0`.
#' @param abs_floor Numeric scalar. Used only when `method = "relative"`.
#'   Minimum absolute change required for flagging. Default: `0.005`.
#'
#' @return Logical vector of the same length as `new_val`.
#'
#' @family change_detection
#' @export
#'
#' @examples
#' flag_changes(c(0.12, 10.5), c(0.10, 10.0), method = "relative")
#' flag_changes(c(0.12, 10.5), c(0.10, 10.0), method = "absolute",
#'              threshold = 0.01)
flag_changes <- function(
  new_val,
  old_val,
  method = c("relative", "absolute", "zscore"),
  threshold = NULL,
  abs_floor = DEFAULT_ABS_FLOOR
) {
  method <- match.arg(method)

  result <- switch(
    method,
    relative = {
      thr <- if (is.null(threshold)) DEFAULT_REL_THRESH else threshold
      flag_relative(new_val, old_val, rel_thresh = thr, abs_floor = abs_floor)
    },
    absolute = {
      thr <- if (is.null(threshold)) DEFAULT_ABS_THRESH else threshold
      flag_absolute(new_val, old_val, abs_thresh = thr)
    },
    zscore = {
      thr <- if (is.null(threshold)) DEFAULT_Z_THRESH else threshold
      flag_zscore(new_val, old_val, z_thresh = thr)
    }
  )
  return(result)
}


#' Add a `_flagged` column to a data.table for a given indicator
#'
#' Convenience wrapper that calls [flag_changes()] on the `.x` (new) and
#' `.y` (old) columns of an indicator and adds the result as
#' `<indicator>_flagged` in place.
#'
#' @param dt A `data.table` with `<indicator>.x` and `<indicator>.y` columns.
#' @param indicator Character scalar. Base name of the indicator
#'   (e.g., `"headcount"`).
#' @param method Character scalar. Passed to [flag_changes()].
#' @param threshold Numeric scalar or `NULL`. Passed to [flag_changes()].
#' @param abs_floor Numeric scalar. Passed to [flag_changes()].
#'
#' @return `dt`, modified by reference, with `<indicator>_flagged` added.
#'
#' @family change_detection
#' @export
#'
#' @examples
#' library(data.table)
#' dt <- data.table(headcount.x = c(0.3, 0.1), headcount.y = c(0.25, 0.1))
#' add_flag_col(dt, "headcount")
#' dt$headcount_flagged
add_flag_col <- function(
  dt,
  indicator,
  method = "relative",
  threshold = NULL,
  abs_floor = DEFAULT_ABS_FLOOR
) {
  col_x <- paste0(indicator, ".x")
  col_y <- paste0(indicator, ".y")
  flag_col <- paste0(indicator, "_flagged")
  dt[,
    (flag_col) := flag_changes(
      get(col_x),
      get(col_y),
      method = method,
      threshold = threshold,
      abs_floor = abs_floor
    )
  ]
  return(dt)
}


#' Apply change flagging to multiple indicators and return a summary
#'
#' For each indicator in `indicators`, applies [flag_changes()] to the
#' merged dataset and returns a summary `data.table` with counts and rates
#' of flagged observations per indicator.
#'
#' @param dt A `data.table` with `<indicator>.x` / `<indicator>.y` column
#'   pairs.
#' @param indicators Character vector of indicator base names.
#' @param method Character scalar. Passed to [flag_changes()].
#' @param threshold Numeric scalar or `NULL`. Passed to [flag_changes()].
#' @param abs_floor Numeric scalar. Passed to [flag_changes()].
#'
#' @return A `data.table` with columns `indicator`, `n_flagged`, `n_total`,
#'   `pct_flagged`.
#'
#' @family change_detection
#' @export
#'
#' @examples
#' library(data.table)
#' dt <- data.table(
#'   headcount.x = c(0.3, 0.1, 0.5),
#'   headcount.y = c(0.25, 0.1, 0.1),
#'   mean.x = c(15, 10, 20),
#'   mean.y = c(10, 10, 10)
#' )
#' summarize_flags(dt, c("headcount", "mean"))
summarize_flags <- function(
  dt,
  indicators,
  method = "relative",
  threshold = NULL,
  abs_floor = DEFAULT_ABS_FLOOR
) {
  rows <- lapply(indicators, function(ind) {
    col_x <- paste0(ind, ".x")
    col_y <- paste0(ind, ".y")

    # Skip indicators not present in this dataset
    if (!all(c(col_x, col_y) %in% names(dt))) {
      return(NULL)
    }

    flags <- flag_changes(
      dt[[col_x]],
      dt[[col_y]],
      method = method,
      threshold = threshold,
      abs_floor = abs_floor
    )
    n_total <- sum(!is.na(flags))
    n_flagged <- sum(flags, na.rm = TRUE)

    data.table(
      indicator = ind,
      n_flagged = n_flagged,
      n_total = n_total,
      pct_flagged = if (n_total > 0L) n_flagged / n_total else NA_real_
    )
  })

  return(rbindlist(Filter(Negate(is.null), rows)))
}
