# ============================================================
# Run this script ONCE locally before deploying to Posit Connect.
# It downloads NEW data from GitHub (via load_from_repo) and
# OLD data from the pipr API for both PPP 2021 and PPP 2017,
# then saves everything as local .fst files.
# ============================================================

library(fastverse)
library(fst)
library(httr)

source("R/utils.R")

ppp_config <- list(
  "2021" = list(pls = c(3, 4.2, 8.3)),
  "2017" = list(pls = c(2.15, 3.65, 6.85))
)

for (ppp_year in names(ppp_config)) {
  pls <- ppp_config[[ppp_year]]$pls
  data_dir <- file.path("data", ppp_year)
  dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)

  message("=== Processing PPP year: ", ppp_year, " ===")

  # --- NEW data from GitHub ---
  message("  Downloading new data from GitHub...")

  agg_new <- load_from_repo("aggregates.fst", ppp_year = ppp_year)
  fst::write_fst(agg_new, file.path(data_dir, "aggregates.fst"))

  syears_new <- load_from_repo("syears.fst", ppp_year = ppp_year)
  fst::write_fst(syears_new, file.path(data_dir, "syears.fst"))

  lyears_new <- load_from_repo("lyears.fst", ppp_year = ppp_year)
  fst::write_fst(lyears_new, file.path(data_dir, "lyears.fst"))

  # --- OLD data from pipr API ---
  message("  Downloading old data from pipr API...")

  dt_agg_old <- lapply(pls, \(.) {
    pipr::get_wb(povline = ., ppp_version = ppp_year) |> qDT()
  }) |>
    rowbind() |>
    setnames("year", "reporting_year")
  fst::write_fst(dt_agg_old, file.path(data_dir, "dt_agg_old.fst"))

  dt_survey_old <- lapply(pls, \(.) {
    pipr::get_stats(povline = ., ppp_version = ppp_year) |> qDT()
  }) |>
    rowbind() |>
    setnames("year", "reporting_year")
  fst::write_fst(dt_survey_old, file.path(data_dir, "dt_survey_old.fst"))

  dt_lineup_old <- lapply(pls, \(.) {
    pipr::get_stats(povline = ., ppp_version = ppp_year, fill_gaps = TRUE) |>
      qDT()
  }) |>
    rowbind() |>
    setnames("year", "reporting_year")
  fst::write_fst(dt_lineup_old, file.path(data_dir, "dt_lineup_old.fst"))

  message("  Done with PPP ", ppp_year)
}

# --- PFW data ---
message("Downloading PFW data...")
pfw <- pipr::get_aux("survey_means") |> qDT()
pfw[, reporting_year := year]
fst::write_fst(pfw, "data/pfw.fst")

message("All deployment data saved successfully to data/")
