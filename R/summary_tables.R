# ============================================================
# summary_tables.R
# Functions that compute ready-to-render summary tables for
# the Quarto Dashboard pages:
#   - build_overview_agg()     → heatmap data for Page 1 (agg level)
#   - build_overview_country() → heatmap data for Page 1 (country level)
#   - build_driver_table()     → which countries drive a regional change
#   - build_source_table()     → explanatory variables for a country-year
#
# Requires: change_detection.R must be sourced before this file.
# Functions used: flag_changes()
# ============================================================

#' Build heatmap data for aggregate-level changes
#'
#' For each `region_code × reporting_year × poverty_line` combination in
#' `agg_merged`, computes the absolute and relative difference for
#' `indicator`, applies the selected significance method, and returns a
#' summary suitable for rendering as a heatmap (rows = regions,
#' columns = years).
#'
#' @param agg_merged `data.table` from `process_ppp_data_extended()$agg_merged`.
#' @param indicator Character scalar. Base indicator name (e.g., `"headcount"`).
#' @param poverty_line Numeric scalar. Poverty line value to filter on.
#' @param method Character scalar. Passed to [flag_changes()]. One of
#'   `"relative"`, `"absolute"`, `"zscore"`.
#' @param threshold Numeric scalar or `NULL`. Significance threshold.
#' @param abs_floor Numeric scalar. Absolute floor for `"relative"` method.
#'
#' @return A `data.table` with columns `region_code`, `reporting_year`,
#'   `<indicator>_new`, `<indicator>_old`, `<indicator>_diff`,
#'   `<indicator>_perc`, `flagged`.
#'
#' @family summary_tables
#' @export
#'
#' @examples
#' \dontrun{
#'   d <- process_ppp_data_extended("2021")
#'   tbl <- build_overview_agg(d$agg_merged, "headcount", poverty_line = 3)
#'   tbl[flagged == TRUE]
#' }
build_overview_agg <- function(
  agg_merged,
  indicator,
  poverty_line,
  method = "relative",
  threshold = NULL,
  abs_floor = 0.005
) {
  col_x <- paste0(indicator, ".x")
  col_y <- paste0(indicator, ".y")
  col_diff <- paste0(indicator, "_diff")
  col_perc <- paste0(indicator, "_perc")

  # Local alias avoids naming conflict between the `poverty_line` parameter
  # and the `poverty_line` column in the data.table i= expression.
  .pl <- poverty_line

  dt <- agg_merged[
    !is.na(region_code) & poverty_line == .pl,
    .(
      region_code,
      reporting_year,
      new_val = get(col_x),
      old_val = get(col_y),
      abs_diff = get(col_diff),
      rel_diff = get(col_perc)
    )
  ]

  # Rename dynamic columns to stable names for the caller
  data.table::setnames(
    dt,
    c("new_val", "old_val", "abs_diff", "rel_diff"),
    c(
      paste0(indicator, "_new"),
      paste0(indicator, "_old"),
      paste0(indicator, "_diff"),
      paste0(indicator, "_perc")
    )
  )

  dt[,
    flagged := flag_changes(
      get(paste0(indicator, "_new")),
      get(paste0(indicator, "_old")),
      method = method,
      threshold = threshold,
      abs_floor = abs_floor
    )
  ]

  return(dt[order(region_code, reporting_year)])
}


#' Build heatmap data for country-level (lineup) changes
#'
#' For each `country_code × reporting_year × poverty_line`, computes change
#' magnitude and flags significant observations in `lineups_merged`. Returns
#' a summary with regional aggregation (count of flagged countries per region).
#'
#' @param lineups_merged `data.table` from
#'   `process_ppp_data_extended()$lineups_merged`.
#' @param indicator Character scalar. Base indicator name.
#' @param poverty_line Numeric scalar. Poverty line value to filter on.
#' @param method Character scalar. Significance method.
#' @param threshold Numeric scalar or `NULL`. Significance threshold.
#' @param abs_floor Numeric scalar. Absolute floor.
#'
#' @return A `data.table` with one row per `region_code × reporting_year`
#'   and columns `n_countries`, `n_flagged`, `pct_flagged`.
#'
#' @family summary_tables
#' @export
#'
#' @examples
#' \dontrun{
#'   d <- process_ppp_data_extended("2021")
#'   tbl <- build_overview_country(d$lineups_merged, "headcount", poverty_line = 3)
#' }
build_overview_country <- function(
  lineups_merged,
  indicator,
  poverty_line,
  method = "relative",
  threshold = NULL,
  abs_floor = 0.005
) {
  col_x <- paste0(indicator, ".x")
  col_y <- paste0(indicator, ".y")

  # Use region_code from the new side (.x); fall back to .y for x-only rows
  region_col <- if ("region_code.x" %in% names(lineups_merged)) {
    "region_code.x"
  } else {
    "region_code"
  }

  .pl <- poverty_line

  dt <- lineups_merged[
    !is.na(country_code) & poverty_line == .pl,
    .(
      country_code,
      region_code = get(region_col),
      reporting_year,
      new_val = get(col_x),
      old_val = get(col_y)
    )
  ]

  dt[,
    flagged := flag_changes(
      new_val,
      old_val,
      method = method,
      threshold = threshold,
      abs_floor = abs_floor
    )
  ]

  # Aggregate to region level
  # pct_flagged denominator uses only rows where flagging was possible (non-NA)
  summary_dt <- dt[
    !is.na(region_code),
    .(
      n_countries  = data.table::uniqueN(country_code),
      n_flagged    = sum(flagged, na.rm = TRUE),
      pct_flagged  = sum(flagged, na.rm = TRUE) / sum(!is.na(flagged))
    ),
    by = .(region_code, reporting_year)
  ]

  return(summary_dt[order(region_code, reporting_year)])
}


#' Build a driver table for a region-year
#'
#' Lists all countries within `region_code` for `reporting_year`, ranked by
#' their weighted contribution to the aggregate change in `headcount`. The
#' weighted contribution is `headcount_diff × population_weight` where
#' `population_weight = reporting_pop.x / sum(reporting_pop.x)`.
#'
#' @param lineups_merged `data.table` from
#'   `process_ppp_data_extended()$lineups_merged`.
#' @param region Character scalar. Region code (e.g., `"EAP"`, `"WLD"`).
#' @param year Numeric scalar. Reporting year.
#' @param poverty_line Numeric scalar. Poverty line value.
#' @param indicator Character scalar. Indicator to attribute. Defaults to
#'   `"headcount"`.
#'
#' @return A `data.table` with columns `country_code`, `country_name`,
#'   `<indicator>_new`, `<indicator>_old`, `<indicator>_diff`,
#'   `pop_weight`, `weighted_contribution`, sorted by
#'   `|weighted_contribution|` descending.
#'
#' @family summary_tables
#' @export
#'
#' @examples
#' \dontrun{
#'   d <- process_ppp_data_extended("2021")
#'   tbl <- build_driver_table(d$lineups_merged, "EAP", 2019, 3)
#' }
build_driver_table <- function(
  lineups_merged,
  region,
  year,
  poverty_line,
  indicator = "headcount"
) {
  col_x <- paste0(indicator, ".x")
  col_y <- paste0(indicator, ".y")
  col_diff <- paste0(indicator, "_diff")

  region_col <- if ("region_code.x" %in% names(lineups_merged)) {
    "region_code.x"
  } else {
    "region_code"
  }

  name_col <- if ("country_name.x" %in% names(lineups_merged)) {
    "country_name.x"
  } else {
    "country_name"
  }

  .pl <- poverty_line

  dt <- lineups_merged[
    get(region_col) == region &
      reporting_year == year &
      poverty_line == .pl &
      !is.na(country_code),
    .(
      country_code,
      country_name = get(name_col),
      new_val = get(col_x),
      old_val = get(col_y),
      indicator_diff = get(col_diff),
      reporting_pop = reporting_pop.x
    )
  ]

  if (nrow(dt) == 0L) {
    return(dt)
  }

  if (!"reporting_pop.x" %in% names(lineups_merged)) {
    stop(
      "Column 'reporting_pop.x' not found in lineups_merged. ",
      "Ensure 'reporting_pop' is present in both new and old lineup data."
    )
  }

  total_pop <- sum(dt$reporting_pop, na.rm = TRUE)
  if (total_pop == 0 || is.na(total_pop)) {
    stop(
      "All reporting_pop values are NA or zero for region '", region,
      "' year ", year, ". Cannot compute population weights."
    )
  }
  dt[,
    `:=`(
      pop_weight = reporting_pop / total_pop,
      weighted_contribution = indicator_diff * (reporting_pop / total_pop)
    )
  ]

  data.table::setnames(
    dt,
    c("new_val", "old_val", "indicator_diff"),
    c(
      paste0(indicator, "_new"),
      paste0(indicator, "_old"),
      paste0(indicator, "_diff")
    )
  )

  return(dt[order(-abs(weighted_contribution))])
}


#' Build a source-of-change table for a country
#'
#' Returns a `data.table` showing changes in explanatory variables
#' (`reporting_gdp`, `reporting_pce`, `reporting_pop`, `cpi`, `ppp`,
#' `welfare_type`, `is_interpolated`, `estimation_type`) for
#' `country_code`, optionally filtered to a year range.
#'
#' @param lineups_merged `data.table` from
#'   `process_ppp_data_extended()$lineups_merged`.
#' @param country Character scalar. Country code.
#' @param poverty_line Numeric scalar. Poverty line value.
#' @param year_range Integer vector of length 2 `c(start, end)`, or `NULL`
#'   for all years. Defaults to `NULL`.
#'
#' @return A `data.table` with one row per year, columns for each
#'   explanatory variable pair (`_new`, `_old`, `_diff` or `_changed`),
#'   sorted by `reporting_year` ascending.
#'
#' @family summary_tables
#' @export
#'
#' @examples
#' \dontrun{
#'   d <- process_ppp_data_extended("2021")
#'   tbl <- build_source_table(d$lineups_merged, "CHN", poverty_line = 3)
#' }
build_source_table <- function(
  lineups_merged,
  country,
  poverty_line,
  year_range = NULL
) {
  # Continuous explanatory variables — include only those present in the data
  cont_vars <- c(
    "reporting_gdp",
    "reporting_pce",
    "reporting_pop",
    "cpi",
    "ppp"
  )
  cont_vars_present <- cont_vars[
    paste0(cont_vars, ".x") %in%
      names(lineups_merged) &
      paste0(cont_vars, ".y") %in% names(lineups_merged)
  ]

  # Categorical explanatory variables
  cat_vars <- c("welfare_type", "estimation_type", "is_interpolated")
  cat_vars_present <- cat_vars[
    paste0(cat_vars, "_changed") %in% names(lineups_merged)
  ]

  cont_new_cols <- paste0(cont_vars_present, ".x")
  cont_old_cols <- paste0(cont_vars_present, ".y")
  cont_diff_cols <- paste0(cont_vars_present, "_diff")
  cat_changed_cols <- paste0(cat_vars_present, "_changed")

  select_cols <- c(
    "reporting_year",
    cont_new_cols,
    cont_old_cols,
    cont_diff_cols,
    cat_changed_cols
  )
  select_cols <- intersect(select_cols, names(lineups_merged))

  .pl <- poverty_line

  dt <- lineups_merged[
    country_code == country & poverty_line == .pl,
    .SD,
    .SDcols = select_cols
  ]

  if (!is.null(year_range)) {
    dt <- dt[reporting_year >= year_range[1] & reporting_year <= year_range[2]]
  }

  return(dt[order(reporting_year)])
}
