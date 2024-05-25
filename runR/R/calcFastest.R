#' Function to calculate fastest time of given distance from gpx file
#'
#' @param gpx output of readGPX
#' @param dist distance or vector of distances to check fastest time
#' @param dist_unit 'k' for kilometers, 'mi' for miles, or 'm' for meters
#'
#' @return
#'
#
#'
#' @export
#'
#'



calcFastest <- function(gpx,
                        dist = 5,
                        dist_unit = "mi") {

  if(!"sf" %in% class(gpx)){
    stop("gpx must be an sf output for readGPX, not an object of class: ", class(gpx))
  }

  gpx <- as.data.frame(gpx)

  if(!all(c("cumdist_mi","cumtime_min") %in% names(gpx))){

    stop("gpx must be an sf output for readGPX. Columns cumdist_mi or cumtime_min were not found.")


  }

  if(dist_unit == "mi"){
    mi <- dist
  } else if(dist_unit == "k"){
    mi <- dist * 0.6214
  } else if(dist_unit == "m"){
    mi <- dist * 0.6214 * 1000
  } else stop("dist_unit must be mi, k or m.")

  max_mi <- max(gpx$cumdist_mi)

  out <- rep(NA,length(dist))

  for(j in 1:length(dist)){

    if(max_mi < mi[j]) {

      out[j] <- NA

    } else{

      times <- rep(NA,nrow(gpx))


      for(i in 1:(nrow(gpx)-1)){
        start_mi <- gpx$cumdist_mi[i]
        start_time <- gpx$cumtime_min[i]


        if((max_mi - start_mi) < mi[j]) {

          times[i] <- NA

          break

        } else{

          cumdist_row <- match(TRUE, (gpx$cumdist_mi - start_mi) >= mi[j])

          times[i] <- gpx$cumtime_min[cumdist_row] - start_time

        }


      }

      out[j] <- min(times,na.rm=TRUE)
    }
  }

  names(out) <- paste0("fastest_",dist,dist_unit)


  return(out)


}
