#### test script - runR



gpxrepo <- "C:/Users/jorda/Desktop/Running/gpx_repo/"


gpx1 <- processGPX(gpx_path = paste0(gpxrepo,
                                     "10000714452.gpx"))


gpx_summary <- updateRepository(gpxrepo,
                                create = TRUE) |>
  calcRecords() |>
  calcRecords(period = "month") |>
  calcRecords(period = "year")
