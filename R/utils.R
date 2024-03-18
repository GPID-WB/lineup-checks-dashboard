load_from_repo <- \(filename) {

  gh_user   <- "https://raw.githubusercontent.com"
  org_data  <- paste(gh_user,
                     "GPID-WB",
                     "lineup-checks-dashboard/data/data",
                     filename,
                     sep = "/")

  temp_file <- tempfile(fileext = fs::path_ext(filename))
  req <- httr::GET(org_data,
                   # write result to disk
                   httr::write_disk(path = temp_file))


  fst::read_fst(temp_file, as.data.table = TRUE)

}



g_trends <- \(dt, country, indicator) {

    g <- ggplot(data = dt,
                aes(x = reporting_year,
                    label = text,
                    group = unique_econ)) +
      geom_line(aes(y = yx, color = "New"), alpha = 0.6) +
      geom_point(aes(y = yx, color = "New"), alpha = 0.6) +
      geom_line(aes(y = yy, color = "Old"), alpha = 0.6) +
      geom_point(aes(y = yy, color = "Old"), alpha = 0.6) +
      scale_color_manual(values = c("New" = "darkgreen",
                                    "Old" = "coral")) +
      theme_classic() +
      labs(title =
             paste0("Compare survey trends in indicators of
                  new and old data for ", country) ,
           x     = "Reporting Year",
           y     = stringr::str_to_title(paste0(indicator)))

  ggplotly(g, tooltip = c("label"))
}