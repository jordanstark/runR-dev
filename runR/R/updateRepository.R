#' Function to update (or create) repository summarizing .gpx files
#'
#' @param repository_path folder containing .gpx files and gpx_summary file
#' @param summary_file name of summary file (default gpx_summary.csv)
#' @param mi_dists vector of distances (in miles) to summarize. Default whole numbers 1:25
#' @param km_dists vector of distances (in km) to summarize. Default whole numbers 1:40
#' @param drop_longer when TRUE (default) removes columns that are all NA because no records are that long. Note these are retained in the .csv file.
#' @param create if FALSE (default) and summary_file is not found, the function will stop. If TRUE, a new summary file will be created.
#' @param verbose if TRUE (default), print a message after each .gpx file is procesed.
#'
#'
#' @returns data frame containing summary of .gpx files. As a side effect, saves this as summary_file.csv. Only updates new files to save time.
#'
#' @export
#'
#'

updateRepository <- function(repository_path,
                             summary_file = "summary_file.csv",
                             mi_dists = 1:25,
                             k_dists = 1:40,
                             drop_longer = TRUE,
                             create = FALSE,
                             verbose = TRUE){

  if(isFALSE(create) &
     !file.exists(paste0(repository_path,summary_file))){

    stop(summary_file,
         " was not found in repository located in ",
         repository_path,
         ". Correct path names or re-run with create = TRUE",
         " to initiate a new repository")

  } else if(isTRUE(create) &
            !file.exists(paste0(repository_path,summary_file))){

    message("Existing summary file was not found. New summary file: ",
            summary_file,
            " is being created.")

    dat <- NULL

    existing_paths <- NULL
    new_paths <- list.files(repository_path,
                            pattern=".gpx",
                            full.names = TRUE)

    if(length(new_paths) == 0){
      stop("No .gpx files found in repository: ",repository_path)
    }

  } else{

    dat <- read.csv(paste0(repository_path,summary_file))
    dat$day <- as.Date(dat$day)
    dat <- dat[order(dat$day),]

    existing_filepaths <- dat$filename

    paths <- list.files(repository_path,
                        pattern=".gpx",
                        full.names = TRUE)


    new_paths <- paths[! basename(paths) %in% existing_filepaths]

    if(length(new_paths) == 0){
      message("No new .gpx files found. Summary file is up to date.")
    } else{

      for(i in 1:length(new_paths)){

        dat <- dat |>
          rbind(processGPX(new_paths[i],
                           mi_dists = mi_dists,
                           k_dists = k_dists))

        if(verbose){
          print(paste0(i," of ",length(new_paths)," completed"))
        }


        dat$day <- as.Date(dat$day)
        dat <- dat[order(dat$day),]


        write.csv(dat,
                  paste0(repository_path,summary_file),
                  row.names = FALSE)


      }


    }
  }



  if(isTRUE(drop_longer)){
    max_mi <- ceiling(max(dat$total_dist_mi))
    max_k <- ceiling(max(dat$total_dist_mi)*1.609)

    over_mi <- paste0("_",
                      max_mi:max(mi_dists),
                      "mi$",
                      collapse="|")
    over_k <- paste0("_",
                     max_k:max(k_dists),
                     "k$",
                     collapse="|")

    dat <- dat[,-which(grepl(over_mi,names(dat)))]
    dat <- dat[,-which(grepl(over_k,names(dat)))]


  }


  return(dat)

}
