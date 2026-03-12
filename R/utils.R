load_from_repo <- \(
  filename,
  ppp_year = c("2021", "2017"),
  data_branch = "main"
) {
  ppp_year <- as.character(ppp_year)
  ppp_year <- match.arg(ppp_year)

  gh_user <- "https://raw.githubusercontent.com"
  org_data <- paste(
    gh_user,
    "GPID-WB",
    "pip-sandbox",
    data_branch,
    "data",
    ppp_year,
    filename,
    sep = "/"
  )

  temp_file <- tempfile(fileext = fs::path_ext(filename))
  req <- httr::GET(
    org_data,
    # write result to disk
    httr::write_disk(path = temp_file)
  )

  fst::read_fst(temp_file, as.data.table = TRUE)
}


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
    (paste0(survey_indicators, "_diff")) := map2(
      mget(names_x),
      mget(names_y),
      `-`
    )
  ]
  surveys_merged[,
    (paste0(survey_indicators, "_perc")) := map2(
      mget(paste0(survey_indicators, "_diff")),
      mget(names_y),
      `/`
    )
  ]
  surveys_merged[,
    (paste0(survey_indicators, "_ratio")) := map2(
      mget(names_x),
      mget(names_y),
      `/`
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
    (paste0(agg_indicators, "_diff")) := map2(
      mget(names_x),
      mget(names_y),
      `-`
    )
  ]
  agg_merged[,
    (paste0(agg_indicators, "_perc")) := map2(
      mget(paste0(agg_indicators, "_diff")),
      mget(names_y),
      `/`
    )
  ]
  agg_merged[,
    (paste0(agg_indicators, "_ratio")) := map2(
      mget(names_x),
      mget(names_y),
      `/`
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

  lineups_merged[,
    (paste0(names_x)) := lapply(.SD, function(x) pmax(x, 0.000001)),
    .SDcols = names_x
  ]
  lineups_merged[,
    (paste0(names_y)) := lapply(.SD, function(x) pmax(x, 0.000001)),
    .SDcols = names_y
  ]

  lineups_merged[,
    (paste0(lineup_indicators, "_diff")) := map2(
      mget(names_x),
      mget(names_y),
      `-`
    )
  ]
  lineups_merged[,
    (paste0(lineup_indicators, "_perc")) := map2(
      mget(paste0(lineup_indicators, "_diff")),
      mget(names_y),
      `/`
    )
  ]
  lineups_merged[,
    (paste0(lineup_indicators, "_ratio")) := map2(
      mget(names_x),
      mget(names_y),
      `/`
    )
  ]

  amnt <- 2
  lineups_merged[,
    (paste0(lineup_indicators, "_thresh_low")) := lapply(.SD, function(x) {
      mn <- mean(x, na.rm = TRUE, trim = 0.01)
      s <- sd(
        x[
          x > quantile(x, 0.01, na.rm = TRUE) &
            x < quantile(x, 0.99, na.rm = TRUE)
        ],
        na.rm = TRUE
      )
      mn - amnt * s
    }),
    .SDcols = paste0(lineup_indicators, "_ratio")
  ]

  lineups_merged[,
    (paste0(lineup_indicators, "_thresh_high")) := lapply(.SD, function(x) {
      mn <- mean(x, na.rm = TRUE, trim = 0.01)
      s <- sd(
        x[
          x > quantile(x, 0.01, na.rm = TRUE) &
            x < quantile(x, 0.99, na.rm = TRUE)
        ],
        na.rm = TRUE
      )
      mn + amnt * s
    }),
    .SDcols = paste0(lineup_indicators, "_ratio")
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
