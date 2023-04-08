
###################################################################################
#' Function that pre-process the data to use with synthReturn
#'
#' @description Function that pre-process the data to use with `synthReturn`
#'
#' @importFrom lubridate is.Date
#' @import data.table
#' @importFrom data.table .N
#' @importFrom data.table ':='
#'
pre_process_synthReturn <- function(
  data,
  tidname,
  cidname,
  rname,
  dname,
  edname,
  estwind,
  eventwind,
  estobs_min,
  eventobs_min,
  placebo,
  ngroup,
  ndraws
){

  #-----------------------------------------------------------------------------
  # Data pre-processing and error checking
  #-----------------------------------------------------------------------------
  # ####

  # # check if `data` is a list: if yes unpack
  # if("list" %in% class(data)){
  #
  #   # make sure dataset is a data.table
  #   # this gets around RStudio's default of reading data as tibble
  #   DT <- base::lapply(data, convert_DT)
  #   r_treat <- DT[[1]]
  #   r_control <- DT[[2]]
  #
  #   # Flag for tidname
  #   if (!is.element(tidname, base::colnames(r_treat))) {
  #     stop("tidname = ",tidname,  " could not be found in the data provided.")
  #   }
  #   # Flag for cidname
  #   if ( !is.element(cidname, base::colnames(r_control))) {
  #     stop("cidname = ",cidname,  " could not be found in the data provided.")
  #   }
  #   # Flag for rname
  #   if ( !is.element(rname, base::colnames(r_treat)) | !is.element(rname, base::colnames(r_control))) {
  #     stop("rname = ",rname,  " could not be found in the data provided.")
  #   }
  #   # Flag for dname
  #   if (!is.element(dname, base::colnames(r_treat)) | !is.element(dname, base::colnames(r_control))) {
  #     stop("dname = ",dname,  " could not be found in the data provided.")
  #   }
  #   # Flag for edname
  #   if (!is.element(edname, base::colnames(r_treat))) {
  #     stop("dname = ",edname,  " could not be found in the data provided.")
  #   }
  #
  #   # Return variable will be denoted by r
  #   colnames(r_treat)[colnames(r_treat) == rname] <- "r"
  #   colnames(r_control)[colnames(r_control) == rname] <- "r"
  #   # date variable will be denoted by d
  #   colnames(r_treat)[colnames(r_treat) == dname] <- "d"
  #   colnames(r_control)[colnames(r_control) == dname] <- "d"
  #   # event date variable will be denoted by ed
  #   colnames(r_treat)[colnames(r_treat) == edname] <- "ed"
  #   # treatment group identifier will be denoted by tid
  #   colnames(r_treat)[colnames(r_treat) == tidname] <- "tid"
  #   # control group identifier will be denoted by cid
  #   colnames(r_control)[colnames(r_control) == cidname] <- "cid"
  #
  #   r_treat <- r_treat[!is.na(tid), c("tid", "d", "ed", "r")]
  #   r_control <- r_control[!is.na(cid), c("cid", "d", "r")]
  #
  #   # sort data with respect to id and time
  #   r_treat <- data.table::setorder(r_treat, tid, d)
  #   r_control <- data.table::setorder(r_control, cid, d)
  #
  # }

  # make sure dataset is a data.table
  # this gets around RStudio's default of reading data as tibble
  DT <- convert_DT(data)

  # Flag for tidname
  if (!is.element(tidname, base::colnames(DT))) {
    stop("tidname = ",tidname,  " could not be found in the data provided.")
  }
  # Flag for cidname
  if ( !is.element(cidname, base::colnames(DT))) {
    stop("cidname = ",cidname,  " could not be found in the data provided.")
  }
  # Flag for rname
  if ( !is.element(rname, base::colnames(DT))) {
    stop("rname = ",rname,  " could not be found in the data provided.")
  }
  # Flag for dname
  if (!is.element(dname, base::colnames(DT))) {
    stop("dname = ",dname,  " could not be found in the data provided.")
  }
  # Flag for edname
  if (!is.element(edname, base::colnames(DT))) {
    stop("edname = ",edname,  " could not be found in the data provided.")
  }

  # check if estimation window has correct format
  if(length(estwind) != 2){
    stop("estwind = ",estwind," is not of format `c(est_start, est_end)`.")
  }

  # check if estimation window has correct format
  if(length(eventwind) != 2){
    stop("eventwind = ",eventwind," is not of format `c(event_start, event_end)`.")
  }


  # Return variable will be denoted by r
  colnames(DT)[colnames(DT) == rname] <- "r"
  # date variable will be denoted by d
  colnames(DT)[colnames(DT) == dname] <- "d"
  # event date variable will be denoted by ed
  colnames(DT)[colnames(DT) == edname] <- "ed"
  # treatment group identifier will be denoted by tid
  colnames(DT)[colnames(DT) == tidname] <- "tid"
  # control group identifier will be denoted by cid
  colnames(DT)[colnames(DT) == cidname] <- "cid"

  # convert data into 2 DTs: r_treat & r_control
  r_treat <- DT[!is.na(DT$tid), c("tid", "d", "ed", "r")]
  r_control <- DT[!is.na(DT$cid), c("cid", "d", "r")]

  # sort data with respect to id and time
  r_treat <- data.table::setorder(r_treat, tid, d)
  r_control <- data.table::setorder(r_control, cid, d)

  # make sure dates are of format date
  if (!(lubridate::is.Date(r_treat[, d])) | !(lubridate::is.Date(r_control[, d]))) {
    stop("data[, dname] must be of format date. Please convert it.")
  }

  # make sure edname is of format date
  if (!(lubridate::is.Date(r_treat[, ed]))) {
    stop("data[, edname] must be of format dname. Please convert it.")
  }

  # make sure IDs are numeric
  if (!(is.numeric(r_treat[, tid])) | !(is.numeric(r_control[, cid]))) {
    stop("data[, c(tidname, cidname)] must be numeric. Please convert it.")

  }

  # make sure returns are numeric
  if (!(is.numeric(r_treat[, r])) | !(is.numeric(r_control[, r]))) {
    stop("data[, rname] must be numeric. Please convert it.")

  }

  # Check if tidname is unique by dname
  n_id_date_treat = base::all(base::table(r_treat[, tid], r_treat[, d]) <= 1)
  if (!n_id_date_treat) stop("The value of tidname must be the unique (by dname)")

  # Check if cidname is unique by dname
  n_id_date_control = base::all( base::table(r_control[, cid], r_control[, d]) <= 1)
  if (! n_id_date_control) stop("The value of tidname must be the unique (by dname)")

  # make sure the estobs_min and eventobs_min are correctly specified
  # get length of estimation and event window
  estwindlen = estwind[2] - estwind[1] + 1
  eventwindlen = eventwind[2] - eventwind[1] + 1

  # check sanity of arguments estobs_min and eventobs_min

  # if estobs_min is greater than 1 and an integer
  if( ((estobs_min > 1) & (estobs_min - base::floor(estobs_min) !=0)) ){
    # est_obsmin has to be smaller than the estimation window
    if(estobs_min > estwindlen){
      stop("The value of estobs_min has to be either between [0,1] or an integer <= length of the estimation window.")
    }
  } else if (estobs_min < 0){
    # estobs_min cannot b smaller than 0
    stop("The value of estobs_min has to be either between [0,1] or an integer <= length of the estimation window.")
  }

  # if eventobs_min is greater than 1 and an integer
  if( ((eventobs_min > 1) & (eventobs_min - base::floor(eventobs_min) !=0)) ){
    # eventobs_min has to be smaller than the event window
    if(eventobs_min > eventwindlen){
      stop("The value of eventobs_min has to be either between [0,1] or an integer <= length of the event window.")
    }
  } else if (eventobs_min < 0){
    # eventobs_min cannot be smaller than 0
    stop("The value of eventobs_min has to be either between [0,1] or an integer <= length of the event window.")
  }


  # check placebo options
  if(placebo == TRUE){
    if(ndraws - base::floor(ndraws) != 0){
      stop("The value of ndraws has to be an integer (>=) 1. Please convert it.")
    }
    if(ngroup <= 1){
      stop("The value of ngroup has to be larger than 1. Please convert it.")
    }
  }

  # if minimum trading days during estimation window in percent, convert to next (smallest) integer
  if(estobs_min >= 0 & estobs_min <= 1){
    estobs_min = base::floor(estobs_min*estwindlen)
  }
  # if minimum trading days during event window in percent, convert to next (smallest) integer
  if(eventobs_min >= 0 & eventobs_min <= 1){
    eventobs_min = base::floor(eventobs_min*eventwindlen)
  }

  #-----------------------------------------------------------------------------
  # setup data in required format

  # get all event dates
  eds <- base::unique(r_treat[, .SD[1], by = tid]$ed)

  # reshape treatment returns

  r_treat <- base::lapply(
    eds,
    get_treat_set,
    tdata = r_treat,
    estwind = estwind,
    eventwind = eventwind,
    estobs_min = estobs_min,
    eventobs_min = eventobs_min
  )
  r_treat <- data.table::rbindlist(r_treat)

  # reshape control returns
  r_control <- base::lapply(
    eds,
    get_control_set,
    cdata = r_control,
    estwind = estwind,
    eventwind = eventwind,
    estobs_min = estobs_min,
    eventobs_min = eventobs_min
  )
  r_control <- data.table::rbindlist(r_control)

  # warning message that there are missing values in treatment group
  if( (base::nrow(r_treat[is.na(r),"tid"]) > 0)){
    tids_na <- r_treat[, .SD[1], by = "tid"]$tid
    message(paste0("Treatment group panel contains missing values for firms with id: ", paste0(as.character(tids_na), collapse = ", "), "."))
  }
  # warning message that there are missing values in panel
  if( (base::nrow(r_control[is.na(r),"cid"]) > 0)){
    cids_na <- r_control[, .SD[1], by = "cid"]$cid
    message(paste0("Control group panel(s) contain missing values. Ids of firms with missing values are: ", paste0(as.character(cids_na), collapse = ", "),". Note that firms with missing values on trading days of treated firm are dropped from control group."))
  }

  out <- list(
    r_treat,
    r_control
  )

  return(out)

}
