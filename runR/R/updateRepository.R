#' Function to update (or create) repository summarizing .gpx files
#'
#' @param repository_path folder containing .gpx files and gpx_summary file
#' @param summary_file name of summary file (default gpx_summary.csv)
#'
#'
#
#'
#' @export
#'
#'

updateRepository <- function(repository_path,
                             summary_file = "summary_file.csv",
                             create = TRUE){

  if(isFALSE(create) &
     !exists(paste0(repository_path,summary_file))){

    stop(summary_file,
         " was not found in repository located in ",
         repository_path,
         ". Correct path names or re-run with create = TRUE",
         " to initiate a new repository")

  } else if(isTRUE(create) &
            !exists(paste0(repository_path,summary_file))){

    dat <- data.frame()

  } else{

    dat <- read.csv(paste0(repository_path,summary_file))

  }

  ## create summary including check for records

}
