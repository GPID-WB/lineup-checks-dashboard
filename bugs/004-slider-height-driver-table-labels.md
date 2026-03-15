# Bug Fix #004: Aggregates Detail — Slider Height, Driver Table, Regional Labels

**Date:** 2026-03-15
**File:** `lineup-checks-revamp.qmd`

---

## Problems & Solutions

### 1. Year Range Slider Too Short

**Problem:** The toolbar row in Aggregates Detail was set to `height="80px"`, which is too short for the Ion.RangeSlider to render legibly — the year labels and drag handles were cut off.

**Solution:** Increased the row height from `80px` to `120px`:
```
## Row {height="120px"}
```

### 2. Driver Table Not Rendering

**Problem:** The driver table was blank (only the explanation text appeared). Root cause: `data.table::setorder(dt, region_code, -abs(weighted_contribution))` fails silently because `setorder` only accepts **column names**, not expressions like `abs(...)`.

**Solution:** Replaced the expression-based sort with a temporary column:
```r
dt[, .abs_wc := abs(weighted_contribution)]
data.table::setorder(dt, region_code, -.abs_wc)
dt[, .abs_wc := NULL]
```

### 3. Regional Comparison Labels Still Showing "true" / "false"

**Problem:** Although `scale_fill_manual()` had `labels = c("TRUE" = "Positive", ...)`, the `ggplotly()` conversion lowercases logical values (`TRUE` → `"true"`), so the label mapping didn't match.

**Solution:** Instead of using a logical fill aesthetic (`aes(fill = change_val > 0)`), created an explicit character column with the desired labels:
```r
agg_latest[, direction := fifelse(change_val > 0, "Positive", "Negative")]
```
Then used `aes(fill = direction)` and `scale_fill_manual(values = c("Positive" = "#06A77D", "Negative" = "#D62828"))`. This bypasses ggplotly's logical-to-string conversion entirely.
