# Dashboard Bugs — Round 3

**Date reported:** 2026-03-15
**File:** `lineup-checks-revamp.qmd`
**Status:** Resolved

---

## Bug 16 — Value boxes cut off on Change Overview

**Problem:** The upper row (`height="130px"`) was too short, causing the
`flagged_agg`, `flagged_lineup`, and `flagged_survey` value boxes to be
visually clipped.

**Fix:** Increased the row height from `130px` to `180px`:
```
## Row {height="180px"}
```

---

## Bug 17 — Heatmap tooltip missing significance method

**Problem:** The plotly heatmap tooltips on the Change Overview page showed
only the entity name, year, and diff value, but did not indicate which change
detection method was used to flag the observation.

**Fix:**
- Added a `method_label` reactive that maps the internal method code to a
  human-readable string (`"relative"` → `"Relative + Abs. Floor"`,
  `"zscore"` → `"Z-Score"`, `"absolute"` → `"Absolute Difference"`).
- Added a `method_label` parameter to `build_plotly_heatmap()`.
- Appended `<br>Method: {method_label}` to the heatmap hover text.
- All three heatmap calls (`heatmap_agg`, `heatmap_lineup`,
  `heatmap_survey`) now pass `method_label = method_label()`.

---

## Bug 18 — Change Overview table column only showed lineups

**Problem:** The right-hand column on the Change Overview page displayed a
single DT table of flagged lineup changes. The user wanted tabs for
aggregates, lineups, and surveys in table form.

**Fix:**
- Replaced the single `### Flagged Lineup Changes {width="50%"}` card with
  a `.tabset` containing three tabs: Aggregates Table, Lineups Table, and
  Surveys Table.
- Added two new server outputs:
  - `tbl_flagged_agg`: DT table of flagged aggregate observations with
    columns `region_code`, `reporting_year`, `old_value`, `new_value`,
    `difference`, `flagged`.
  - `tbl_flagged_surveys`: DT table of flagged survey observations with
    columns `country_code`, `reporting_year`, `survey_year`, `old_value`,
    `new_value`, `difference`, `flagged`.
- Both new tables have `filter = "top"` and default-filter on
  `flagged == "true"`, matching the existing lineup table behavior.

---

## Bug 19 — "colour:old" / "colour:new" in aggregate time series tooltip

**Problem:** The aggregate time series chart (`plot_agg_ts`) used
`ggplot2::aes(color = "New")` and `color = "Old"` for the two lines. When
converted via `ggplotly()`, the tooltip displayed `colour: New` or
`colour: Old` as raw aesthetic labels.

**Fix:** Added `tooltip = c("x", "y")` to the `ggplotly()` call so only the
year and indicator value are shown on hover. The line identity (old vs new)
is visible from the legend and line style (solid vs dashed).

Applied the same fix to `plot_country_ts` (country lineup time series) to
prevent the same issue with `colour:` and `linetype:` labels.

---

## Bug 20 — Regional comparison bar chart blank

**Problem:** `plot_agg_bars` rendered blank or errored in some cases. The root
cause was that the computed column was named `diff`, which shadows
`base::diff()`. In ggplot2's tidy evaluation context, `reorder(region_code,
-abs(diff))` and `aes(fill = diff > 0)` could resolve `diff` as the base R
function instead of the data column, producing unexpected results or errors.

**Fix:** Renamed the column from `diff` to `change_val`:
```r
agg_latest[, change_val := get(ind_x) - get(ind_y)]
```
Updated all `aes()` references to use `change_val`.

---

## Bug 21 — Driver table missing reporting_year column

**Problem:** The driver table output did not include `reporting_year`, making
it unclear which year the data pertained to.

**Fix:** Added `reporting_year` to the `.(...)` column selection in the
driver table server code.

Additionally, added explanatory text above the DT output in the layout
section describing what the driver table shows and how to interpret the
`weighted_contribution` column.

---

## Bug 22 — Country Detail tables in wrong positions

**Problem:** In the Country Detail page, `tbl_source` (source of change) was
in the sidebar and `tbl_survey` (survey data comparison) was in the main
column. The user wanted them swapped: `tbl_source` in the main column and
`tbl_survey` in the sidebar.

**Fix:** Swapped the positions:
- **Sidebar:** country selector → year range → survey coverage changes →
  survey data comparison (`tbl_survey`)
- **Main column:** lineup time series chart → source of change (`tbl_source`)
