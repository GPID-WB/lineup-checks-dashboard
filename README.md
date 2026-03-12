# Lineup Checks Dashboard

Interactive Shiny flexdashboard that compares **new** PIP pipeline outputs against **old** (published) data from the pipr API. Supports both PPP 2021 and PPP 2017.

## Prerequisites

- R ≥ 4.1
- Required packages: `flexdashboard`, `shiny`, `shinyWidgets`, `ggplot2`, `plotly`, `RColorBrewer`, `purrr`, `fst`, `fastverse`, `joyn`, `pipr`, `httr`

## Project structure

```
R/
  save_deploy_data.R   # Step 1: downloads all data locally
  utils.R              # Helper functions (load_from_repo, process_ppp_data, etc.)
  get_new_data.R       # Legacy script (not used)
data/
  2021/                # PPP 2021 data (.fst files)
  2017/                # PPP 2017 data (.fst files)
  pfw.fst              # Survey framework proxy (from pipr survey_means)
lineup-checks-dashboard.Rmd   # The dashboard
```

## How to run

### 1. Download the data

Run `R/save_deploy_data.R` once to fetch all data and save it locally:

```r
source("R/save_deploy_data.R")
```

This will:

- Download **new** data (aggregates, survey years, lineup years) from the [GPID-WB/pip-sandbox](https://github.com/GPID-WB/pip-sandbox) GitHub repo for both PPP 2021 and PPP 2017.
- Download **old** data from the pipr API (`get_wb`, `get_stats`, `get_stats(fill_gaps = TRUE)`) with the corresponding `ppp_version` for each PPP year.
- Download PFW proxy data from `pipr::get_aux("survey_means")`.
- Save everything as `.fst` files under `data/2021/`, `data/2017/`, and `data/pfw.fst`.

### 2. Run the dashboard

Open `lineup-checks-dashboard.Rmd` and click **Run Document**, or:

```r
rmarkdown::run("lineup-checks-dashboard.Rmd")
```

The dashboard reads the pre-saved `.fst` files — no network access is needed at runtime.

### 3. Deploy to Posit Connect

```r
rsconnect::deployDoc("lineup-checks-dashboard.Rmd")
```

Make sure the `data/` folder (with both PPP subfolders and `pfw.fst`) is included in the deployment bundle.

## Updating the data

Re-run `source("R/save_deploy_data.R")` whenever a new pipeline release is available, then redeploy.
