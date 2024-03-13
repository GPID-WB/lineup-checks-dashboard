# Prepare-----------------------------------------------------------------------
source("R/utils.R")
pl <- 2.15

# Aggregates--------------------------------------------------------------------
dt_agg_old <- pipr::get_wb(povline = pl) |>
  qDT()
dt_agg_new <- load_from_repo("aggregates.fst")
dt_agg_old <- dt_agg_old |>
  frename(reporting_year = year)

# Survey years------------------------------------------------------------------
dt_survey_old <- pipr::get_stats(povline = pl) |>
  qDT()
dt_survey_new <- load_from_repo("syears.fst")
dt_survey_old <- dt_survey_old |>
  frename(reporting_year = year)

# aggregates--------------------------------------------------------------------
dt_lineup_old <- pipr::get_stats(povline = pl,
                                 fill_gaps = TRUE) |>
  qDT()
dt_lineup_old <- dt_lineup_old |>
  frename(reporting_year = year)
dt_lineup_new <- load_from_repo("lyears.fst")

# Save data---------------------------------------------------------------------
write_fst(x = dt_agg_old,
          path = here::here("data", "use", "dt_agg_old.fst"))
write_fst(x = dt_agg_new,
          path = here::here("data", "use", "dt_agg_new.fst"))
write_fst(x = dt_survey_old,
          path = here::here("data", "use", "dt_survey_old.fst"))
write_fst(x = dt_survey_new,
          path = here::here("data", "use", "dt_survey_new.fst"))
write_fst(x = dt_lineup_old,
          path = here::here("data", "use", "dt_lineup_old.fst"))
write_fst(x = dt_lineup_new,
          path = here::here("data", "use", "dt_lineup_new.fst"))
