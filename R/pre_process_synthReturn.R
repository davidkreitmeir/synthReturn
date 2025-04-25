
###################################################################################
#' Function that pre-process the data to use with synthReturn
#'
#' @description Function that pre-process the data to use with `synthReturn`
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
  ndraws,
  ncores,
  is_windows
) {

  #-----------------------------------------------------------------------------
  # Data pre-processing and error checking
  #-----------------------------------------------------------------------------
  # ####

  # make sure dataset is a data.table
  # this gets around RStudio's default of reading data as tibble
  # copy data (inefficient but permits by-reference operations without affecting input data)
  # copying should be omitted in future patch (requires downstream changes)
  if(("data.table" %chin% class(data))) {
    DT <- data.table::copy(data)
  } else {
    DT <- data.table::as.data.table(data)
  }

  names_DT <- names(DT)
  # Flag for tidnam
  if(!(tidname %chin% names_DT)) {
    stop("tidname = ", tidname, " could not be found in the data provided.")
  }
  # Flag for cidname
  if(!(cidname %chin% names_DT)) {
    stop("cidname = ", cidname, " could not be found in the data provided.")
  }
  # Flag for rname
  if(!(rname %chin% names_DT)) {
    stop("rname = ", rname, " could not be found in the data provided.")
  }
  # Flag for dname
  if(!(dname %chin% names_DT)) {
    stop("dname = ", dname, " could not be found in the data provided.")
  }
  # Flag for edname
  if(!(edname %chin% names_DT)) {
    stop("edname = ", edname, " could not be found in the data provided.")
  }
  rm(names_DT)

  # check if estimation window has correct format
  if(!is.vector(estwind) || length(estwind) != 2L) {
    stop("estwind = ", estwind, " is not of format `c(est_start, est_end)`.")
  }
  if(anyNA(estwind)) {
    stop("estwind must not contain NA values.")
  }
  if(estwind[1L] > estwind[2L]) {
    stop("estwind[1] must not be larger than estwind[2]")
  }

  # check if estimation window has correct format
  if(!is.vector(eventwind) || length(eventwind) != 2L) {
    stop("eventwind = ", eventwind, " is not of format `c(event_start, event_end)`.")
  }
  if(anyNA(eventwind)) {
    stop("eventwind must not contain NA values.")
  }
  if(eventwind[1L] > eventwind[2L]) {
    stop("eventwind[1] must not be larger than eventwind[2]")
  }

  # Return variable will be denoted by r
  # date variable will be denoted by d
  # event date variable will be denoted by ed
  # treatment group identifier will be denoted by tid
  # control group identifier will be denoted by cid
  data.table::setnames(DT, c(rname, dname, edname, tidname, cidname), c("r", "d", "ed", "tid", "cid"))

  # convert data into 2 DTs: r_treat & r_control
  r_treat <- DT[!is.na(tid), c("tid", "d", "ed", "r")]
  r_control <- DT[!is.na(cid), c("cid", "d", "r")]

  # sort data with respect to id and time
  r_treat <- data.table::setorder(r_treat, tid, d)
  r_control <- data.table::setorder(r_control, cid, d)

  # make sure dates are of format date
  if(!(inherits(DT[["d"]], "Date"))) {
    stop("data[, dname] must be of format date. Please convert it.")
  }

  # make sure edname is of format date
  if(!(inherits(r_treat[["ed"]], "Date"))) {
    stop("data[, edname] must be of format dname. Please convert it.")
  }

  # make sure IDs are numeric
  if(!(is.numeric(r_treat[["tid"]])) || !(is.numeric(r_control[["cid"]]))) {
    stop("data[, c(tidname, cidname)] must be numeric. Please convert it.")
  }

  # make sure returns are numeric
  if(!(is.numeric(DT[["r"]]))) {
    stop("data[, rname] must be numeric. Please convert it.")
  }

  # Check if tidname is unique by dname
  if(sum(r_treat[, .(dup = anyDuplicated(tid)), by = "d"][["dup"]]) != 0L) {
    stop("The value of tidname must be the unique (by dname)")
  }

  # Check if cidname is unique by dname
  if(sum(r_control[, .(dup = anyDuplicated(cid)), by = "d"][["dup"]]) != 0L) {
    stop("The value of tidname must be the unique (by dname)")
  }

  # make sure the estobs_min and eventobs_min are correctly specified
  # get length of estimation and event window
  estwindlen <- estwind[2L] - estwind[1L] + 1L
  eventwindlen <- eventwind[2L] - eventwind[1L] + 1L

  # check sanity of arguments estobs_min and eventobs_min

  if(length(estobs_min) != 1L || !is.numeric(estobs_min) || !is.finite(estobs_min)) stop("estobs_min must be numeric and of length one.")
  if(length(eventobs_min) != 1L || !is.numeric(eventobs_min) || !is.finite(eventobs_min)) stop("eventobs_min must be numeric and of length one.")

  # if estobs_min is greater than 1 and not an integer
  if((estobs_min > 1L) && (as.integer(estobs_min) != estobs_min)) {
    # est_obsmin has to be smaller than the estimation window
    if(estobs_min > estwindlen){
      stop("The value of estobs_min has to be either between [0,1] or an integer <= length of the estimation window.")
    }
  } else if(estobs_min < 0L) {
    # estobs_min cannot b smaller than 0
    stop("The value of estobs_min has to be either between [0,1] or an integer <= length of the estimation window.")
  }

  # if eventobs_min is greater than 1 and an integer
  if((eventobs_min > 1L) && (as.integer(eventobs_min) != eventobs_min)) {
    # eventobs_min has to be smaller than the event window
    if(eventobs_min > eventwindlen) {
      stop("The value of eventobs_min has to be either between [0,1] or an integer <= length of the event window.")
    }
  } else if(eventobs_min < 0L) {
    # eventobs_min cannot be smaller than 0
    stop("The value of eventobs_min has to be either between [0,1] or an integer <= length of the event window.")
  }

  # check placebo options
  if(placebo){
    if(length(ndraws) != 1L || !is.numeric(ndraws) || !is.finite(ndraws)) {
      stop("ndraws must be numeric and of length one.")
    }
    if(ndraws < 1L || as.integer(ndraws) != ndraws){
      stop("The value of ndraws has to be an integer (>=) 1. Please convert it.")
    }
    if(length(ngroup) != 1L || !is.numeric(ngroup) || !is.finite(ngroup)) {
      stop("ngroup must be numeric and of length one.")
    }
    if(ngroup <= 1L) {
      stop("The value of ngroup has to be larger than 1. Please convert it.")
    }
  }

  # if minimum trading days during estimation window in percent, convert to next (smallest) integer
  if(estobs_min %between% (0:1)) {
    estobs_min <- floor(estobs_min * estwindlen)
  }
  # if minimum trading days during event window in percent, convert to next (smallest) integer
  if(eventobs_min %between% (0:1)) {
    eventobs_min <- floor(eventobs_min * eventwindlen)
  }

  #-----------------------------------------------------------------------------
  # setup data in required form

  # reshape treatment returns
  r_treat <- split(r_treat, by = "ed")
  if(ncores == 1L) {
    r_treat <- data.table::rbindlist(
      lapply(
        r_treat,
        get_treat_set,
        estwind = estwind,
        eventwind = eventwind,
        estobs_min = estobs_min,
        eventobs_min = eventobs_min
      )
    )
  } else {
    if(is_windows) {
      mirai::daemons(ncores)
      r_treat <- data.table::rbindlist(
        mirai::mirai_map(
          r_treat,
          get_treat_set,
          .args = list(
            estwind = estwind,
            eventwind = eventwind,
            estobs_min = estobs_min,
            eventobs_min = eventobs_min
          )
        )[]
      )
    } else {
      r_treat <- data.table::rbindlist(
        parallel::mclapply(
          r_treat,
          get_treat_set,
          estwind = estwind,
          eventwind = eventwind,
          estobs_min = estobs_min,
          eventobs_min = eventobs_min,
          mc.cores = ncores
        )
      )
    }
  }

  # reshape control returns
  if(ncores == 1L) {
    r_control <- data.table::rbindlist(
      lapply(
        unique(r_treat[, "ed"])[["ed"]],
        get_control_set,
        cdata = r_control,
        estwind = estwind,
        eventwind = eventwind,
        estobs_min = estobs_min,
        eventobs_min = eventobs_min
      )
    )
  } else {
    if(is_windows) {
      r_control <- data.table::rbindlist(
        mirai::mirai_map(
          unique(r_treat[, "ed"])[["ed"]],
          get_control_set,
          .args = list(
            cdata = r_control,
            estwind = estwind,
            eventwind = eventwind,
            estobs_min = estobs_min,
            eventobs_min = eventobs_min
          )
        )[]
      )
      mirai::daemons(0L)
    } else {
      r_control <- data.table::rbindlist(
        parallel::mclapply(
          unique(r_treat[, "ed"])[["ed"]],
          get_control_set,
          cdata = r_control,
          estwind = estwind,
          eventwind = eventwind,
          estobs_min = estobs_min,
          eventobs_min = eventobs_min,
          mc.cores = ncores
        )
      )
    }
  }

  # warning message that there are missing values in treatment group
  if(anyNA(r_treat[["r"]])){
    tids_na <- unique(r_treat[is.na(r), "tid"])[["tid"]]
    warning(paste0("Treatment group panel contains missing values for firms with id: ", paste0(tids_na, collapse = ", "), "."))
  }
  # warning message that there are missing values in panel
  if(anyNA(r_control[["r"]])){
    cids_na <- unique(r_control[is.na(r), "cid"])[["cid"]]
    warning(paste0("Control group panel(s) contain missing values. Ids of firms with missing values are: ", paste0(cids_na, collapse = ", "), ". Note that",
      " firms with missing values on trading days of treated firm are dropped from control group."))
  }

  out <- list(
    r_treat,
    r_control
  )

  return(out)
}
