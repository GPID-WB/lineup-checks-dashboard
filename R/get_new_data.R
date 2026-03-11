hours <- 0
minutes <- hours * 60
seconds <- minutes * 60

Sys.sleep(seconds)


library(fastverse)
library(pipapi)
options(pipapi.query_live_data = TRUE)
getOption("pipapi.query_live_data")
# pak::pak("PIP-technical-team/pipapi@DEV")
# pak::pak("PIP-technical-team/wbpip@DEV")
# remotes::install_github("PIP-technical-team/pipapi@new-test-suite")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                   Subfunctions   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                   Set up   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# v1 <- "20230919_2017_01_02_PROD"
ver <- "20260324_2021_01_02_PROD"

# data_pipeline <-  fs::path("//w1wbgencifs01/pip/pip_ingestion_pipeline/pc_data/output-tfs-sync/ITSES-POVERTYSCORE-DATA")

data_pipeline <- fs::path("e:/PIP/pipapi_data/")

lkups <- pipapi::create_versioned_lkups(
  data_pipeline,
  # vintage_pattern = "20230919_2017_01_02_PROD"
  vintage_pattern = ver
)

lkup <- lkups$versions_paths[[lkups$latest_release]]

# Compare two different version -----------

## survey data -----------

ctr <- "all"
pl <- c(2.15, 3.65, 6.85)
pl <- c(3, 4.2, 8.3) # for 2021 ppp

pip2_cl <-
  pip(country = ctr, lkup = lkup, povline = pl) |>
  setorder(
    country_code,
    reporting_year,
    reporting_level,
    welfare_type,
    poverty_line
  )


# fst::write_fst(pip1_cl, fs::path(compare_dir,"syears/pip_old", ext = "fst" ))
fst::write_fst(pip2_cl, fs::path("data", "syears", ext = "fst"))
haven::write_dta(pip2_cl, fs::path("data", "syears", ext = "dta"))


# waldo::compare(pip1, pip2)

## lineup data ----------
pip2 <- pip(
  country = ctr,
  fill_gaps = TRUE,
  lkup = lkup,
  povline = pl
) |>
  setorder(
    country_code,
    reporting_year,
    reporting_level,
    welfare_type,
    poverty_line
  )


fst::write_fst(pip2, fs::path("data", "lyears", ext = "fst"))
haven::write_dta(pip2, fs::path("data", "lyears", ext = "dta"))


pip2_g <- pip_grp_logic(
  country = ctr,
  lkup = lkup,
  povline = .,
  group_by = "wb"
) |>
  setorder(region_code, reporting_year, poverty_line)


fst::write_fst(pip2_g, fs::path("data", "aggregates", ext = "fst"))
haven::write_dta(pip2_g, fs::path("data", "aggregates", ext = "dta"))


writeLines(
  format(Sys.time(), "%F %T"),
  fs::path("data", "data_update_timestamp", ext = "txt")
)

ga()
gca("update")
gp()
