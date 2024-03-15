
library(fastverse)

# pak::pak("PIP-technical-team/pipapi@DEV")
# pak::pak("PIP-technical-team/wbpip@DEV")
# pak::pak("PIP-technical-team/pipapi@PROD")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                   Subfunctions   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                   Set up   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# v1 <- "20230919_2017_01_02_PROD"
ver <- "20240326_2017_01_02_PROD"

# data_pipeline <-  fs::path("//w1wbgencifs01/pip/pip_ingestion_pipeline/pc_data/output-tfs-sync/ITSES-POVERTYSCORE-DATA")

data_pipeline <-  fs::path("e:/PIP/pipapi_data/")

lkups <- pipapi::create_versioned_lkups(
  data_pipeline,
  # vintage_pattern = "20230919_2017_01_02_PROD"
  vintage_pattern = ver
)

lkup <- lkups$versions_paths[[lkups$latest_release]]

# Compare two different version -----------

## survey data -----------

ctr <- "all"
pl <- 2.15
pip2_cl   <- pipapi::pip(country = ctr,
                         lkup    = lkup,
                         povline = pl)

# fst::write_fst(pip1_cl, fs::path(compare_dir,"syears/pip_old", ext = "fst" ))
fst::write_fst(pip2_cl, fs::path("data","syears", ext = "fst" ))
haven::write_dta(pip2_cl, fs::path("data","syears", ext = "dta" ))


# waldo::compare(pip1, pip2)


## lineup data ----------

pip2   <- pipapi::pip(country   = ctr,
                      fill_gaps = TRUE,
                      lkup      = lkup,
                      povline   = pl)  |>
  setorder(country_code, reporting_year, reporting_level, welfare_type)

fst::write_fst(pip2, fs::path("data", "lyears", ext = "fst" ))
haven::write_dta(pip2, fs::path("data", "lyears", ext = "dta" ))


pip2_g   <- pipapi::pip_grp_logic(country = ctr,
                            lkup = lkup,
                            povline = pl,
                            group_by = "wb")

fst::write_fst(pip2_g, fs::path("data", "aggregates", ext = "fst" ))
haven::write_dta(pip2_g, fs::path("data", "aggregates", ext = "dta" ))


writeLines(format(Sys.time(), "%F %T") ,
           fs::path("data",
                    "data_update_timestamp",
                    ext = "txt"))


