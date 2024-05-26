#' Function to read in .gpx file and calculate time and distance
#'
#' @param gpx_path Full path to .gpx file
#'
#' @return sf object with added columns relating to distance and time
#'
#
#'
#' @export
#'
#'


readGPX <- function(gpx_path){

  if(!grepl(".gpx$",gpx_path)){
    stop("File: ",gpx_path," is not a .gpx file.")
  }

  out <- sf::st_read(gpx_path,
                     "track_points",
                     quiet = TRUE)

  out$diff_m <- sf::st_distance(out$geometry,
                                dplyr::lag(out$geometry),
                                by_element=TRUE)

  out$diff_sec <- out$time - dplyr::lag(out$time)

  out$dist_mi <- ifelse(is.na(out$diff_m),
                        0,
                        as.numeric(out$diff_m)/1609.344)

  out$time_min <- ifelse(is.na(out$diff_sec),
                         0,
                         as.numeric(out$diff_sec)/60)

  out$cumdist_mi <- cumsum(out$dist_mi)

  out$cumtime_min <- cumsum(out$time_min)

  return(out)

}

