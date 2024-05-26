#' Function to process GPX files to summarize records
#'
#' @param gpx_path filepath to .gpx file
#' @param mi_dists vector of distances (in miles) to summarize. Default whole numbers 1:25
#' @param km_dists vector of distances (in km) to summarize. Default whole numbers 1:40
#'
#' @return data frame summarizing .gpx input
#'
#
#'
#' @export
#'
#'

processGPX <- function(gpx_path,
                       mi_dists = 1:25,
                       k_dists = 1:40){

  dat <- readGPX(gpx_path)


  gpx_summary <- data.frame(filename = basename(gpx_path),
                            day = lubridate::as_date(dat$time[1]),
                            total_dist_mi = max(dat$cumdist_mi),
                            total_time_min = max(dat$cumtime_min),
                            t(calcFastest(dat,
                                        dist = mi_dists,
                                        dist_unit = "mi")),
                            t(calcFastest(dat,
                                          dist = k_dists,
                                          dist_unit = "k")))


}
