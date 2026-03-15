# ============================================================
# process_data.R
# Extended data processing for the lineup checks dashboard.
# Builds on the original process_ppp_data() in utils.R by:
#   - Harmonizing old-format column names to new conventions.
#   - Carrying explanatory variables (reporting_gdp, reporting_pce,
#     reporting_pop, cpi, ppp, welfare_type, is_interpolated,
#     estimation_type) through the merge as .x / .y suffix pairs.
#   - Computing diffs for explanatory variables.
#   - Detecting new/dropped survey coverage.
# ============================================================

#' Harmonize column names in old PIP pipeline data
#'
#' Renames legacy column names (`gdp`, `hfce`, `pop`) from the old PIP API
#' output to match the naming convention in the new pipeline output
#' (`reporting_gdp`, `reporting_pce`, `reporting_pop`). Modifies `dt` in
#' place.
#'
#' @param dt A `data.table` loaded from one of the `dt_*_old.fst` files.
#'
#' @return The same `data.table`, modified by reference, with harmonized
#'   column names.
#'
#' @family process_data
#' @export
#'
#' @examples
#' dt <- data.table::data.table(gdp = 100, hfce = 50, pop = 1e6,
#'                               headcount = 0.3)
#' harmonize_old_cols(dt)
#' names(dt) # gdp -> reporting_gdp, etc.
harmonize_old_cols <- function(dt) {
  if (!data.table::is.data.table(dt)) {
    stop("`dt` must be a data.table, got ", class(dt)[[1L]])
  }
  col_map <- c(
    gdp = "reporting_gdp",
    hfce = "reporting_pce",
    pop = "reporting_pop"
  )
  existing <- intersect(names(col_map), names(dt))
  if (length(existing) > 0L) {
    data.table::setnames(dt, old = existing, new = col_map[existing])
  }
  return(dt)
}


#' Compute absolute, relative, and ratio differences between two columns
#'
#' Adds three new columns to `dt` in place for each variable in `vars`:
#' `<var>_diff` (absolute), `<var>_perc` (relative to old), and
#' `<var>_ratio` (new / old). Division by zero is guarded with a small
#' epsilon floor.
#'
#' @param dt A `data.table` that already contains columns `<var>.x` (new) and
#'   `<var>.y` (old) for each element of `vars`.
#' @param vars Character vector of base variable names (without `.x`/`.y`).
#' @param epsilon Numeric scalar used as the denominator floor to prevent
#'   division by zero. Defaults to `1e-9`.
#'
#' @return `dt`, modified by reference, with `_diff`, `_perc`, and `_ratio`
#'   columns added.
#'
#' @family process_data
#' @export
#'
#' @examples
#' library(data.table)
#' dt <- data.table(mean.x = c(10, 5), mean.y = c(8, 0))
#' add_diff_cols(dt, "mean")
#' dt[, .(mean_diff, mean_perc, mean_ratio)]
add_diff_cols <- function(dt, vars, epsilon = 1e-9) {
  names_x <- paste0(vars, ".x")
  names_y <- paste0(vars, ".y")

  # absolute difference: new - old
  dt[,
    (paste0(vars, "_diff")) := Map(`-`, mget(names_x), mget(names_y))
  ]
  # relative difference: (new - old) / |old|  (epsilon guard; NaN-safe)
  dt[,
    (paste0(vars, "_perc")) := Map(
      function(d, o) {
        o_safe <- ifelse(is.nan(o), NA_real_, o)
        d / pmax(abs(o_safe), epsilon)
      },
      mget(paste0(vars, "_diff")),
      mget(names_y)
    )
  ]
  # ratio: new / old  (epsilon guard; NaN-safe)
  dt[,
    (paste0(vars, "_ratio")) := Map(
      function(n, o) {
        o_safe <- ifelse(is.nan(o), NA_real_, o)
        n / pmax(abs(o_safe), epsilon)
      },
      mget(names_x),
      mget(names_y)
    )
  ]
  return(dt)
}


#' Detect changes in categorical explanatory variables after a merge
#'
#' For each variable in `cat_vars`, adds a logical `<var>_changed` column
#' that is `TRUE` when the `.x` (new) and `.y` (old) values differ.
#' `NA` comparisons are treated as no change (both `NA` → `FALSE`;
#' one `NA` → `TRUE`).
#'
#' @param dt A `data.table` with `<var>.x` and `<var>.y` columns.
#' @param cat_vars Character vector of variable names to compare.
#'
#' @return `dt`, modified by reference, with `<var>_changed` logical
#'   columns added.
#'
#' @family process_data
#' @export
#'
#' @examples
#' library(data.table)
#' dt <- data.table(welfare_type.x = c("consumption", "income"),
#'                  welfare_type.y = c("consumption", "consumption"))
#' add_categorical_change_cols(dt, "welfare_type")
#' dt$welfare_type_changed
add_categorical_change_cols <- function(dt, cat_vars) {
  for (v in cat_vars) {
    col_x <- paste0(v, ".x")
    col_y <- paste0(v, ".y")
    # Both NA → no change; one NA → changed; different values → changed
    dt[,
      (paste0(v, "_changed")) := fifelse(
        is.na(get(col_x)) & is.na(get(col_y)),
        FALSE,
        fifelse(
          is.na(get(col_x)) | is.na(get(col_y)),
          TRUE,
          get(col_x) != get(col_y)
        )
      )
    ]
  }
  return(dt)
}


#' Detect new and dropped survey coverage between pipeline versions
#'
#' Compares survey-year-country-level-welfare_type combinations present in
#' the new vs old survey data. Returns a `data.table` flagging additions
#' and removals.
#'
#' @param dt_survey_new `data.table` from `syears.fst` (new pipeline).
#' @param dt_survey_old `data.table` from `dt_survey_old.fst` (old pipeline),
#'   already column-harmonized via [harmonize_old_cols()].
#'
#' @return A `data.table` with columns `country_code`, `reporting_year`,
#'   `reporting_level`, `welfare_type`, and `change` (character: `"added"` or
#'   `"dropped"`).
#'
#' @family process_data
#' @export
#'
#' @examples
#' \dontrun{
#'   cov <- detect_survey_coverage_changes(dt_survey_new, dt_survey_old)
#'   cov[change == "added"]
#' }
detect_survey_coverage_changes <- function(dt_survey_new, dt_survey_old) {
  keys <- c("country_code", "reporting_year", "reporting_level", "welfare_type")

  # Validate key columns exist in both tables
  missing_new <- setdiff(keys, names(dt_survey_new))
  missing_old <- setdiff(keys, names(dt_survey_old))
  if (length(missing_new) > 0L) {
    stop(
      "dt_survey_new missing key columns: ",
      paste(missing_new, collapse = ", ")
    )
  }
  if (length(missing_old) > 0L) {
    stop(
      "dt_survey_old missing key columns: ",
      paste(missing_old, collapse = ", ")
    )
  }

  # Deduplicate first to avoid materialising a full copy before unique()
  new_keys <- unique(dt_survey_new, by = keys)[, .SD, .SDcols = keys]
  old_keys <- unique(dt_survey_old, by = keys)[, .SD, .SDcols = keys]

  added <- fsetdiff(new_keys, old_keys)[, change := "added"]
  dropped <- fsetdiff(old_keys, new_keys)[, change := "dropped"]

  return(rbindlist(list(added, dropped)))
}


#' Load and process PIP pipeline data for one PPP year (extended)
#'
#' Reads new and old `.fst` files for `ppp_year`, harmonizes legacy column
#' names, merges each data type (survey, lineup, aggregates), computes
#' indicator differences (absolute, relative, ratio), and adds comparisons
#' for explanatory variables (`reporting_gdp`, `reporting_pce`,
#' `reporting_pop`, `cpi`, `ppp`, `welfare_type`, `is_interpolated`,
#' `estimation_type`) to support source-of-change attribution.
#'
#' @param ppp_year Character scalar. PPP reference year: `"2021"` or `"2017"`.
#' @param data_dir Character scalar. Root directory containing `<ppp_year>/`
#'   subdirectories. Defaults to `"data"`.
#'
#' @return A named list with elements:
#'   \describe{
#'     \item{surveys_merged}{Merged survey-year `data.table`.}
#'     \item{lineups_merged}{Merged lineup-year `data.table`.}
#'     \item{agg_merged}{Merged aggregate `data.table`.}
#'     \item{survey_indicators}{Character vector of survey indicator names.}
#'     \item{lineup_indicators}{Character vector of lineup indicator names.}
#'     \item{agg_indicators}{Character vector of aggregate indicator names.}
#'     \item{expl_vars}{Character vector of continuous explanatory variable
#'       names (with diffs computed).}
#'     \item{cat_expl_vars}{Character vector of categorical explanatory
#'       variable names (with `_changed` flag computed).}
#'     \item{countries_available}{Sorted character vector of country codes
#'       present in surveys.}
#'     \item{regions_available}{Sorted character vector of region codes
#'       present in aggregates.}
#'     \item{dup_countries}{Country codes with duplicated survey entries.}
#'     \item{dt_survey_new}{Unmerged new survey data (for PFW comparisons).}
#'     \item{survey_coverage_diff}{`data.table` of added/dropped survey
#'       country-year-level-welfare combinations.}
#'   }
#'
#' @family process_data
#' @export
#'
#' @examples
#' \dontrun{
#'   d <- process_ppp_data_extended("2021")
#'   d$survey_coverage_diff[change == "added"]
#' }
process_ppp_data_extended <- function(ppp_year, data_dir = "data") {
  ppp_year <- as.character(ppp_year)
  data_path <- file.path(data_dir, ppp_year)

  # --- Load data ---
  dt_agg_old <- fst::read_fst(
    file.path(data_path, "dt_agg_old.fst"),
    as.data.table = TRUE
  )
  dt_agg_new <- fst::read_fst(
    file.path(data_path, "aggregates.fst"),
    as.data.table = TRUE
  )
  dt_survey_old <- fst::read_fst(
    file.path(data_path, "dt_survey_old.fst"),
    as.data.table = TRUE
  )
  dt_survey_new <- fst::read_fst(
    file.path(data_path, "syears.fst"),
    as.data.table = TRUE
  )
  dt_lineup_old <- fst::read_fst(
    file.path(data_path, "dt_lineup_old.fst"),
    as.data.table = TRUE
  )
  dt_lineup_new <- fst::read_fst(
    file.path(data_path, "lyears.fst"),
    as.data.table = TRUE
  )

  # --- Harmonize old column names ---
  harmonize_old_cols(dt_survey_old)
  harmonize_old_cols(dt_lineup_old)
  # Note: agg old uses `pop` for region population, but new uses `reporting_pop`
  if ("pop" %in% names(dt_agg_old)) {
    data.table::setnames(dt_agg_old, "pop", "reporting_pop")
  }

  # --- Define indicator groups ---
  # Indicators: the outcome variables we're comparing
  # Enumerate explicitly to avoid positional range selection breaking on schema changes
  EXPECTED_INDICATORS <- c(
    "headcount",
    "poverty_gap",
    "poverty_severity",
    "watts",
    "mean",
    "median",
    "mld",
    "gini",
    "polarization",
    paste0("decile", 1:10),
    "spl",
    "spr"
  )
  survey_indicators <- intersect(EXPECTED_INDICATORS, names(dt_survey_new))
  lineup_indicators <- intersect(EXPECTED_INDICATORS, names(dt_lineup_new))
  agg_indicators <- intersect(EXPECTED_INDICATORS, names(dt_agg_new))

  if (length(survey_indicators) == 0L) {
    stop(
      "No expected indicator columns found in dt_survey_new. ",
      "Check that syears.fst contains standard PIP indicator columns."
    )
  }

  # Explanatory variables (continuous): compute diff/perc/ratio
  expl_vars <- c(
    "reporting_gdp",
    "reporting_pce",
    "reporting_pop",
    "cpi",
    "ppp"
  )

  # Categorical explanatory variables: compute _changed flag
  cat_expl_vars <- c("welfare_type", "estimation_type")

  # Boolean explanatory variable
  bool_expl_vars <- "is_interpolated"

  # ---- Survey merge ---------------------------------------------------
  survey_join_keys <- c(
    "country_code",
    "reporting_year",
    "poverty_line",
    "welfare_type",
    "reporting_level"
  )
  surveys_merged <- joyn::joyn(
    x = dt_survey_new,
    y = dt_survey_old,
    by = survey_join_keys,
    keep = "full",
    suffixes = c(".x", ".y"),
    keep_common_vars = TRUE,
    reportvar = ".joyn",
    verbose = FALSE
  )
  surveys_merged[,
    unique_econ := paste0(country_code, reporting_level, welfare_type)
  ]
  add_diff_cols(surveys_merged, survey_indicators)
  # Explanatory vars: only those present in both after merge
  sv_expl_present <- intersect(
    expl_vars,
    gsub("\\.x$", "", grep("\\.x$", names(surveys_merged), value = TRUE))
  )
  add_diff_cols(surveys_merged, sv_expl_present)
  add_categorical_change_cols(
    surveys_merged,
    intersect(
      c(cat_expl_vars, bool_expl_vars),
      gsub("\\.x$", "", grep("\\.x$", names(surveys_merged), value = TRUE))
    )
  )

  # ---- Lineup merge ---------------------------------------------------
  lineup_join_keys <- c(
    "country_code",
    "reporting_year",
    "poverty_line",
    "welfare_type",
    "reporting_level"
  )
  lineups_merged <- joyn::joyn(
    x = dt_lineup_new,
    y = dt_lineup_old,
    by = lineup_join_keys,
    keep = "full",
    suffixes = c(".x", ".y"),
    keep_common_vars = TRUE,
    reportvar = ".joyn",
    verbose = FALSE
  )
  lineups_merged[,
    unique_econ := paste0(country_code, reporting_level, welfare_type)
  ]
  # Guard against zero/negative values before ratio computation
  names_x <- paste0(lineup_indicators, ".x")
  names_y <- paste0(lineup_indicators, ".y")
  lineups_merged[,
    (names_x) := lapply(.SD, function(v) pmax(v, 1e-9)),
    .SDcols = names_x
  ]
  lineups_merged[,
    (names_y) := lapply(.SD, function(v) pmax(v, 1e-9)),
    .SDcols = names_y
  ]
  add_diff_cols(lineups_merged, lineup_indicators)
  lp_expl_present <- intersect(
    expl_vars,
    gsub("\\.x$", "", grep("\\.x$", names(lineups_merged), value = TRUE))
  )
  add_diff_cols(lineups_merged, lp_expl_present)
  add_categorical_change_cols(
    lineups_merged,
    intersect(
      c(cat_expl_vars, bool_expl_vars),
      gsub("\\.x$", "", grep("\\.x$", names(lineups_merged), value = TRUE))
    )
  )

  # ---- Aggregate merge ------------------------------------------------
  agg_join_keys <- c("region_code", "poverty_line", "reporting_year")
  agg_merged <- joyn::joyn(
    x = dt_agg_new,
    y = dt_agg_old,
    by = agg_join_keys,
    keep = "full",
    suffixes = c(".x", ".y"),
    keep_common_vars = TRUE,
    reportvar = ".joyn",
    verbose = FALSE
  )
  agg_merged[, unique_econ := region_code]
  add_diff_cols(agg_merged, agg_indicators)

  # ---- Survey coverage changes ----------------------------------------
  survey_coverage_diff <- detect_survey_coverage_changes(
    dt_survey_new,
    dt_survey_old
  )

  # ---- Metadata -------------------------------------------------------
  countries_available <- collapse::funique(surveys_merged$country_code)
  regions_available <- collapse::funique(
    collapse::na_omit(agg_merged$region_code)
  ) |>
    sort()

  dup_countries <- surveys_merged[,
    .N,
    by = c("country_code", "reporting_year", "poverty_line")
  ][N > 1, ]$country_code |>
    collapse::funique()

  return(list(
    surveys_merged = surveys_merged,
    lineups_merged = lineups_merged,
    agg_merged = agg_merged,
    survey_indicators = survey_indicators,
    lineup_indicators = lineup_indicators,
    agg_indicators = agg_indicators,
    expl_vars = expl_vars,
    cat_expl_vars = c(cat_expl_vars, bool_expl_vars),
    countries_available = countries_available,
    regions_available = regions_available,
    dup_countries = dup_countries,
    dt_survey_new = dt_survey_new,
    survey_coverage_diff = survey_coverage_diff
  ))
}
