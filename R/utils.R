#' Load an .fst data file from the GPID-WB/pip-sandbox GitHub repository
#'
#' Downloads a file from the pip-sandbox repository at the given PPP year
#' subdirectory and reads it as a `data.table`. Uses the GitHub Contents API
#' via the `gh` package, which authenticates automatically via
#' `GITHUB_TOKEN` / `GITHUB_PAT` environment variables or `gitcreds`.
#'
#' For files ≤ 1 MB the API returns base64-encoded content directly.
#' For larger files it returns a `download_url` which is used as a fallback.
#'
#' @param filename Character scalar. Name of the `.fst` file to load
#'   (e.g., `"aggregates.fst"`).
#' @param ppp_year Character scalar. PPP reference year: `"2021"` or `"2017"`.
#' @param data_branch Character scalar. Git branch to download from.
#'   Defaults to `"main"`.
#'
#' @return A `data.table` containing the loaded data.
#'
#' @family utils
#' @export
#'
#' @examples
#' \dontrun{
#'   agg <- load_from_repo("aggregates.fst", ppp_year = "2021")
#'   agg <- load_from_repo("aggregates.fst", ppp_year = "2021", data_branch = "dev")
#' }
load_from_repo <- \(
  filename,
  ppp_year = c("2021", "2017"),
  data_branch = getOption("pipsandbox.branch", "main")
) {
  ppp_year <- as.character(ppp_year)
  ppp_year <- match.arg(ppp_year)

  # Update GH_ORG / GH_REPO if the repo moves
  GH_ORG <- "GPID-WB"
  GH_REPO <- "pip-sandbox"
  path <- paste("data", ppp_year, filename, sep = "/")

  # GitHub Contents API — authenticated via GITHUB_TOKEN / gitcreds
  resp <- gh::gh(
    "GET /repos/{owner}/{repo}/contents/{path}",
    owner = GH_ORG,
    repo = GH_REPO,
    path = path,
    ref = data_branch
  )

  temp_file <- tempfile(fileext = paste0(".", fs::path_ext(filename)))

  # Files ≤ 1 MB: content is base64-encoded in the response.
  # Files > 1 MB: content is NULL; fall back to download_url.
  content_b64 <- resp[["content"]]
  if (!is.null(content_b64) && nzchar(content_b64)) {
    writeBin(jsonlite::base64_dec(content_b64), temp_file)
  } else {
    dl_url <- resp[["download_url"]]
    if (is.null(dl_url)) {
      rlang::abort(paste0(
        "Cannot download '",
        filename,
        "' from branch '",
        data_branch,
        "': ",
        "GitHub API returned neither content nor a download_url."
      ))
    }
    utils::download.file(
      dl_url,
      destfile = temp_file,
      mode = "wb",
      quiet = TRUE
    )
  }

  fst::read_fst(temp_file, as.data.table = TRUE)
}


#' Plot trend lines comparing old and new pipeline estimates
#'
#' Creates a `ggplotly`-interactive line chart showing the old vs new
#' indicator values over time for a given country. Each line corresponds
#' to one `unique_econ` (country-level-welfare combination).
#'
#' @param dt A `data.table` with columns `reporting_year`, `text`
#'   (tooltip label), `unique_econ` (line grouping), `yx` (new values),
#'   and `yy` (old values).
#' @param country Character scalar. Country name used in the plot title.
#' @param indicator Character scalar. Indicator name used as the y-axis label.
#'
#' @return A `plotly` object.
#'
#' @family utils
#' @export
#'
#' @examples
#' \dontrun{
#'   g_trends(dt_country, "China", "headcount")
#' }
g_trends <- \(dt, country, indicator) {
  g <- ggplot(
    data = dt,
    aes(x = reporting_year, label = text, group = unique_econ)
  ) +
    geom_line(aes(y = yx, color = "New"), alpha = 0.6) +
    geom_point(aes(y = yx, color = "New"), alpha = 0.6) +
    geom_line(aes(y = yy, color = "Old"), alpha = 0.6) +
    geom_point(aes(y = yy, color = "Old"), alpha = 0.6) +
    scale_color_manual(values = c("New" = "darkgreen", "Old" = "coral")) +
    theme_classic() +
    labs(
      title = paste0(
        "Compare survey trends in indicators of
                  new and old data for ",
        country
      ),
      x = "Reporting Year",
      y = stringr::str_to_title(paste0(indicator))
    )

  ggplotly(g, tooltip = c("label"))
}


#' Load and process PIP pipeline data for one PPP year (legacy)
#'
#' @description
#' **Deprecated.** Use [process_ppp_data_extended()] from `process_data.R`
#' instead. This function is retained for compatibility with the legacy
#' `.Rmd` dashboard only.
#'
#' Reads old and new `.fst` files for `ppp_year`, merges each data type
#' (survey, lineup, aggregates), and computes `_diff`, `_perc`, `_ratio`
#' columns for all indicators.
#'
#' @param ppp_year Character scalar. PPP reference year: `"2021"` or `"2017"`.
#'
#' @return A named list with elements `surveys_merged`, `agg_merged`,
#'   `lineups_merged`, `survey_indicators`, `agg_indicators`,
#'   `lineup_indicators`, `countries_available`, `regions_available`,
#'   `dup_countries`, `dt_survey_new`.
#'
#' @family utils
#' @seealso [process_ppp_data_extended()]
#'
#' @examples
#' \dontrun{
#'   d <- process_ppp_data("2021")
#' }
process_ppp_data <- function(ppp_year) {
  data_dir <- file.path("data", ppp_year)

  dt_agg_old <- fst::read_fst(
    file.path(data_dir, "dt_agg_old.fst"),
    as.data.table = TRUE
  )
  dt_agg_new <- fst::read_fst(
    file.path(data_dir, "aggregates.fst"),
    as.data.table = TRUE
  )
  dt_survey_old <- fst::read_fst(
    file.path(data_dir, "dt_survey_old.fst"),
    as.data.table = TRUE
  )
  dt_survey_new <- fst::read_fst(
    file.path(data_dir, "syears.fst"),
    as.data.table = TRUE
  )
  dt_lineup_old <- fst::read_fst(
    file.path(data_dir, "dt_lineup_old.fst"),
    as.data.table = TRUE
  )
  dt_lineup_new <- fst::read_fst(
    file.path(data_dir, "lyears.fst"),
    as.data.table = TRUE
  )

  # survey data ---------------------
  survey_indicators <- dt_survey_new |>
    fselect(headcount:decile10, spl, spr) |>
    colnames()

  surveys_merged <- join(
    x = dt_survey_new,
    y = dt_survey_old,
    on = c(
      "country_code",
      "reporting_year",
      "poverty_line",
      "welfare_type",
      "reporting_level"
    ),
    how = "full",
    verbose = FALSE,
    suffix = c(".x", ".y"),
    column = list(".joyn", c("x", "y", "x & y"))
  )

  names_x <- paste0(survey_indicators, ".x")
  names_y <- paste0(survey_indicators, ".y")

  surveys_merged[,
    (paste0(survey_indicators, "_diff")) := Map(
      `-`,
      mget(names_x),
      mget(names_y)
    )
  ]
  surveys_merged[,
    (paste0(survey_indicators, "_perc")) := Map(
      function(d, o) d / pmax(abs(o), 1e-9),
      mget(paste0(survey_indicators, "_diff")),
      mget(names_y)
    )
  ]
  surveys_merged[,
    (paste0(survey_indicators, "_ratio")) := Map(
      function(n, o) n / pmax(abs(o), 1e-9),
      mget(names_x),
      mget(names_y)
    )
  ]
  surveys_merged[,
    unique_econ := paste0(country_code, reporting_level, welfare_type)
  ]

  # aggregate data ---------------------
  agg_indicators <- dt_agg_new |>
    fselect(headcount:spr) |>
    colnames()

  agg_merged <- join(
    x = dt_agg_new,
    y = dt_agg_old,
    on = c("region_code", "poverty_line", "reporting_year"),
    how = "full",
    verbose = FALSE,
    suffix = c(".x", ".y"),
    column = list(".joyn", c("x", "y", "x & y"))
  )

  names_x <- paste0(agg_indicators, ".x")
  names_y <- paste0(agg_indicators, ".y")

  agg_merged[,
    (paste0(agg_indicators, "_diff")) := Map(
      `-`,
      mget(names_x),
      mget(names_y)
    )
  ]
  agg_merged[,
    (paste0(agg_indicators, "_perc")) := Map(
      function(d, o) d / pmax(abs(o), 1e-9),
      mget(paste0(agg_indicators, "_diff")),
      mget(names_y)
    )
  ]
  agg_merged[,
    (paste0(agg_indicators, "_ratio")) := Map(
      function(n, o) n / pmax(abs(o), 1e-9),
      mget(names_x),
      mget(names_y)
    )
  ]
  agg_merged[, unique_econ := paste0(region_code)]

  # Line up year -------------
  lineup_indicators <- dt_lineup_new |>
    fselect(headcount:decile10, spl, spr) |>
    colnames()

  lineups_merged <- join(
    x = dt_lineup_new,
    y = dt_lineup_old,
    on = c(
      "country_code",
      "reporting_year",
      "poverty_line",
      "welfare_type",
      "reporting_level"
    ),
    how = "full",
    verbose = FALSE,
    suffix = c(".x", ".y"),
    column = list(".joyn", c("x", "y", "x & y"))
  )

  names_x <- paste0(lineup_indicators, ".x")
  names_y <- paste0(lineup_indicators, ".y")

  # Do NOT floor raw data — apply epsilon guard only inside the denominators
  lineups_merged[,
    (paste0(lineup_indicators, "_diff")) := Map(
      `-`,
      mget(names_x),
      mget(names_y)
    )
  ]
  lineups_merged[,
    (paste0(lineup_indicators, "_perc")) := Map(
      function(d, o) d / pmax(abs(o), 1e-9),
      mget(paste0(lineup_indicators, "_diff")),
      mget(names_y)
    )
  ]
  lineups_merged[,
    (paste0(lineup_indicators, "_ratio")) := Map(
      function(n, o) n / pmax(abs(o), 1e-9),
      mget(names_x),
      mget(names_y)
    )
  ]

  # Compute both threshold bounds in a single lapply pass to avoid
  # recomputing quantile(), mean(), and sd() twice per column
  ZSCORE_MULTIPLIER <- 2L # ±2 SD band for outlier threshold
  lineups_merged[,
    c(
      paste0(lineup_indicators, "_thresh_low"),
      paste0(lineup_indicators, "_thresh_high")
    ) := {
      ratio_cols <- mget(paste0(lineup_indicators, "_ratio"))
      bounds <- lapply(ratio_cols, function(x) {
        qs <- quantile(x, c(0.01, 0.99), na.rm = TRUE)
        trimmed <- x[x > qs[[1L]] & x < qs[[2L]]]
        mn <- mean(x, na.rm = TRUE, trim = 0.01)
        s <- sd(trimmed, na.rm = TRUE)
        list(mn - ZSCORE_MULTIPLIER * s, mn + ZSCORE_MULTIPLIER * s)
      })
      list(
        lapply(bounds, `[[`, 1L),
        lapply(bounds, `[[`, 2L)
      )
    }
  ]

  lineups_merged[,
    unique_econ := paste0(country_code, reporting_level, welfare_type)
  ]

  countries_available <- surveys_merged$country_code |> funique()
  regions_available <- agg_merged$region_code |>
    funique() |>
    na_omit() |>
    sort()
  dup_countries <- surveys_merged[,
    .N,
    by = c("country_code", "reporting_year", "poverty_line")
  ][N > 1, ]$country_code |>
    funique()

  list(
    surveys_merged = surveys_merged,
    agg_merged = agg_merged,
    lineups_merged = lineups_merged,
    survey_indicators = survey_indicators,
    agg_indicators = agg_indicators,
    lineup_indicators = lineup_indicators,
    countries_available = countries_available,
    regions_available = regions_available,
    dup_countries = dup_countries,
    dt_survey_new = dt_survey_new
  )
}
