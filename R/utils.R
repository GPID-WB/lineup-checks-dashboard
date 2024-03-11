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

