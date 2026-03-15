# Lineup Checks Dashboard

Interactive Quarto Dashboard with Shiny server that compares **new** PIP pipeline outputs against **old** (published) data. The dashboard surfaces significant changes across three data levels (aggregates, lineup years, survey years), lets users adjust sensitivity thresholds, and attributes the source of changes using explanatory variables (GDP, consumption, population, CPI, welfare type).

Supports both **PPP 2021** and **PPP 2017**.

## Prerequisites

- R ≥ 4.1
- Quarto ≥ 1.4
- Required packages:
  - Core: `data.table`, `fst`, `fastverse`, `joyn`, `here`
  - Visualization: `ggplot2`, `plotly`
  - Shiny + Dashboard: `shiny`, `bslib`, `bsicons`, `htmltools`, `reactable`
  - Testing: `testthat`

Install missing packages:

```r
pkgs <- c(
  "data.table", "fst", "fastverse", "joyn", "here",
  "ggplot2", "plotly", "shiny", "bslib", "bsicons",
  "htmltools", "reactable", "testthat"
)
install.packages(setdiff(pkgs, rownames(installed.packages())))
```

## Project Structure

```
R/
  process_data.R                   # Extended data processing (merge + explanatory vars)
  change_detection.R               # Significance flagging (relative, z-score, absolute)
  summary_tables.R                 # Overview aggregation (driver tables, source tables)
  save_deploy_data.R               # Download data pipeline
  utils.R                          # Legacy helper functions
tests/
  testthat.R                       # Test runner
  testthat/
    test-process_data.R            # 33 unit + integration tests
    test-change_detection.R        # 29 tests (relative, z-score, absolute flagging)
    test-summary_tables.R          # 21 tests (aggregation logic)
    _snaps/                        # Test snapshots
data/
  2021/                            # PPP 2021 data (.fst files)
    syears.fst                     # Survey year-level data (new)
    lyears.fst                     # Lineup year-level data (new)
    aggregates.fst                 # Regional/global aggregates (new)
    dt_survey_old.fst              # Survey year-level data (old)
    dt_lineup_old.fst              # Lineup year-level data (old)
    dt_agg_old.fst                 # Regional/global aggregates (old)
  2017/                            # PPP 2017 data (same structure as above)
  pfw.fst                          # Survey framework proxy (from pipr)
lineup-checks-revamp.qmd           # NEW: Quarto Dashboard with Shiny server
lineup-checks-dashboard.Rmd        # OLD: flexdashboard (preserved for reference)
```

## How to Run

### 1. Download Data (First Time Only)

Run `R/save_deploy_data.R` to fetch and cache all data locally:

```r
source(here::here("R", "save_deploy_data.R"))
```

This downloads:
- **New data**: aggregates, survey years, lineup years from [GPID-WB/pip-sandbox](https://github.com/GPID-WB/pip-sandbox) for PPP 2021 & 2017
- **Old data**: from pipr API (`get_wb`, `get_stats`, `get_stats(fill_gaps = TRUE)`)
- **Survey metadata**: from `pipr::get_aux("survey_means")`

All data is cached as `.fst` files under `data/2021/`, `data/2017/`, and `data/pfw.fst`.

### 2. Run Tests (Optional)

Verify all R code is working:

```r
testthat::test_dir(here::here("tests", "testthat"), reporter = "summary")
```

Expected output: **82 tests passing** (33 process_data + 29 change_detection + 22 summary_tables - 2 intentionally skipped)

### 3. Run the New Dashboard

Open `lineup-checks-revamp.qmd` and click **Preview**, or:

```r
quarto::quarto_preview("lineup-checks-revamp.qmd")
```

The dashboard will load both PPP years and launch an interactive Shiny server. Navigate between four pages:
1. **Change Overview** — Summary value boxes, regional heatmap, flagged countries table
2. **Aggregates Detail** — Regional time series, driver table (countries ranked by contribution), bar chart
3. **Country Detail** — Country selector, lineup/survey time series, explanatory variable changes, survey coverage
4. **All Indicators** — Scatter plot (old vs new), Bland-Altman plot, flagged outliers

### 4. Adjust Sensitivity

Use the global sidebar to:
- Select **PPP Version** (2021 or 2017)
- Choose **Significance Method**:
  - Relative + Absolute Floor _(default)_ — flags when `|change|/|old| > threshold` AND `|change| > floor`
  - Z-Score — flags when standardized change exceeds threshold
  - Absolute Difference — flags when `|change| > threshold`
- Drag **Threshold** slider (0–10) to adjust sensitivity
- Set **Absolute Floor** (Relative method only, 0–0.05)
- Pick **Indicator** (headcount, mean on main pages; all indicators on Page 4)
- Select **Poverty Line** (1.90, 3.20, 5.50 USD/day)

## Updating Data

When a new PIP release is available, re-run the data download:

```r
source(here::here("R", "save_deploy_data.R"))
```

Then re-run the dashboard (no code changes needed).

## Deployment

### Local/Positron

```r
quarto::quarto_preview("lineup-checks-revamp.qmd")
```

### Posit Connect

```r
rsconnect::deployDoc("lineup-checks-revamp.qmd")
```

Ensure `data/` (with both PPP subfolders and `pfw.fst`) and all R scripts are in the deployment bundle.

## Code Quality

All R code follows standards in `.github/instructions/r.instructions.md`:
- snake_case naming
- roxygen2 documentation (exported functions)
- Explicit `return()` statements
- `rlang::abort()` for error handling
- No print/cat statements (functions return objects)
- data.table + tidyverse patterns

## Key Functions

### Data Processing (`R/process_data.R`)
- `process_ppp_data_extended(ppp_year, data_dir)` — Load and merge old/new data with explanatory variables
- `harmonize_old_cols(dt)` — Rename old column names (`gdp` → `reporting_gdp`, etc.)
- `add_diff_cols(dt, vars, epsilon)` — Compute `_diff`, `_perc`, `_ratio` columns
- `detect_survey_coverage_changes(dt_survey_new, dt_survey_old)` — Identify new/dropped surveys

### Change Detection (`R/change_detection.R`)
- `flag_relative(new, old, rel_thresh, abs_floor)` — Relative difference with floor
- `flag_absolute(new, old, abs_thresh)` — Absolute difference only
- `flag_zscore(new, old, z_thresh, trim)` — Z-score of changes
- `flag_changes(new, old, method, threshold, abs_floor)` — Dispatcher
- `add_flag_col(dt, indicator, method, threshold, abs_floor)` — Add flag column in-place

### Summary Tables (`R/summary_tables.R`)
- `build_overview_agg(agg_merged, indicator, poverty_line, method, threshold, abs_floor)` — Regions × years heatmap
- `build_overview_country(lineups_merged, indicator, poverty_line, method, threshold, abs_floor)` — Country-level overview
- `build_driver_table(lineups_merged, region, year, poverty_line, indicator)` — Countries ranked by contribution
- `build_source_table(lineups_merged, country, poverty_line, year_range)` — Explanatory variable changes

## Example: Adjusting Sensitivity

1. Open the dashboard
2. Set **Significance Method** to "Relative + Absolute Floor"
3. Drag **Threshold** to 10% (from default 5%)
4. Adjust **Absolute Floor** to 0.01 (from default 0.005)
5. The value boxes, heatmap, and tables update instantly to show only flagged changes that are both >10% relative change AND >0.01 absolute change
