#' Function to read in .gpx file and calculate time and distance
#'
#' @param filepath Full path to .gpx file
#'
#' @return sf object with added columns relating to distance and time
#'
#
#'
#' @export
#'
#'


readGPX <- function(filepath){

  if(!grepl(".gpx$",filepath)){
    stop("File: ",filepath," is not a .gpx file.")
  }

  out <- st_read(filepath,
                 "track_points",
                 quiet = TRUE)

  out$diff_m <- st_distance(out$geometry,
                            lag(out$geometry),
                            by_element=TRUE)

  out$diff_sec <- out$time - lag(out$time)

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

