#' Function to update (or create) repository summarizing .gpx files
#'
#' @param dat summary file created by updateRepository
#' @param period period over which to check for records. Currently supports 'all', 'month', or 'year.
#'
#' @returns data frame containing summary of .gpx files with
#' added columns specifying whether a given row was a record when it occurred.
#'
#' @export
#'
#'

calcRecords <- function(dat,
                        period = "all"){

  if(period == "all"){

    vec_names <- names(dat)[which(grepl("^fastest_",
                                        names(dat)))]

    for(vec in vec_names){


      dat[[paste0("record_",vec)]] <- flagRecords(dat[[vec]])

    }




  } else if(period == "month"){

    my <- substr(dat$day,1,7)

    vec_names <- names(dat)[which(grepl("^fastest_",
                                        names(dat)))]

    dl <- dat |>
      split(my)

    for(i in seq_along(dl)){
      for(vec in vec_names){

        dl[[i]][[paste0("monthlyrecord_",vec)]] <- flagRecords(dl[[i]][[vec]])

      }


    }

    dat <- do.call(rbind,dl)

  } else if(period == "year"){

    y <- substr(dat$day,1,4)

    vec_names <- names(dat)[which(grepl("^fastest_",
                                        names(dat)))]

    dl <- dat |>
      split(y)

    for(i in seq_along(dl)){
      for(vec in vec_names){

        dl[[i]][[paste0("yearlyrecord_",vec)]] <- flagRecords(dl[[i]][[vec]])

      }


    }

    dat <- do.call(rbind,dl)

  } else {
    stop("'period' must be 'all', 'month', or 'year', not ",period)
  }



  return(dat)


}

flagRecords <- function(vec){

  newrec <- rep(NA,length(vec))

  for(i in 1:length(vec)){

    if(is.na(vec[i])) next

    if(all(is.na(vec[1:(i-1)]))){
      newrec[i] <- "Yes"
      next
    }

    if(i == 1) {
      newrec[i] <- "Yes"
      next
    }

    newrec[i] <- ifelse(vec[i] < min(vec[1:(i-1)],na.rm=TRUE),
                        "Yes","No")

  }

  return(newrec)

}
