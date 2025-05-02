
###################################################################################
#' Function that pre-process the data to use with synthReturn
#'
#' @description Function that pre-process the data to use with `synthReturn`
#'
pre_process_synthReturn <- function(
  DT,
  unitname,
  treatname,
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
  # copy data (inefficient but permits by-reference operations without affecting input data)
  # copying should be omitted in future patch (requires downstream changes)
  if(!("data.table" %chin% class(DT))) {
    DT <- data.table::as.data.table(DT)
  }

  names_DT <- names(DT)
  # Flag for unit_id
  if(!(unitname %chin% names_DT)) {
    stop("unitname = ", unitname, " could not be found in the data provided.")
  }
  # Flag for treat
  if(!(treatname %chin% names_DT)) {
    stop("treatname = ", treatname, " could not be found in the data provided.")
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
  if(estwind[2L] >= 0L) {
    warning("estwind incorporates time periods >= 0. Thus, it does not exclusively select control units based on pre-treatment time periods. This is ",
      "technically allowed, but usually not intended.")
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

  if(estwind[2L] >= eventwind[1L]) {
    warning("The estwind interval does not strictly preceed the eventwind interval. Usually, you want the estimation window to refer only to time periods ",
      "before the event window.")
  }

  # make sure IDs are numeric
  if(!is.numeric(DT[[unitname]])) {
    stop("data[, unitname] must be numeric. Please convert it.")
  }

  # make sure treatment indicator is logical
  if(!is.logical(DT[[treatname]])) {
    stop("data[, unitname] must be numeric. Please convert it.")
  }

  # make sure dates are of format date
  if(!is.numeric(DT[[dname]])) {
    stop("data[, dname] must be of format date. Please convert it.")
  }

  # make sure edname is of format date
  if(!is.numeric(DT[[edname]])) {
    stop("data[, edname] must be of format dname. Please convert it.")
  }

  # make sure returns are numeric
  if(!(is.numeric(DT[[rname]]))) {
    stop("data[, rname] must be numeric. Please convert it.")
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

  # Check if unit_id is unique by dname
  if(anyDuplicated(DT[, c(dname, unitname)]) != 0) {
    stop("The value of unitname must be the unique (by dname). I.e. a unit must not be observed more than once per time period.")
  }

  # Drop NA treat rows
  if(anyNA(DT[[treatname]])) {
    nr_DT_pre <- nrow(DT)
    DT <- na.omit(DT, cols = treatname)
    warning("Dropping ", nr_DT_pre - nrow(DT), " rows because of NA values in ", treatname, ".")
  }

  # convert data into 2 DTs: r_treat & r_control
  r_treat <- DT[(DT[[treatname]]), c(..unitname, ..dname, ..edname, ..rname)]
  r_control <- DT[!(DT[[treatname]]), c(..unitname, ..dname, ..rname)]

  # Check row numbers
  if(nrow(r_treat) == 0L) {
    stop("The treatment group does not contain any observations.")
  }
  if(nrow(r_control) == 0L) {
    stop("The control group does not contain any observations.")
  }

  # Return variable will be denoted by r
  # date variable will be denoted by d
  # event date variable will be denoted by ed
  # treatment group identifier will be denoted by tid
  # control group identifier will be denoted by cid
  data.table::setnames(r_treat, c(rname, dname, edname, unitname), c("r", "d", "ed", "unit_id"))
  data.table::setnames(r_control, c(rname, dname, unitname), c("r", "d", "unit_id"))

  # Subset to finite values
  nr_DT_pre <- nrow(r_treat)
  r_treat <- r_treat[is.finite(r) & is.finite(d) & is.finite(ed) & is.finite(unit_id),]
  nr_DT <- nrow(r_treat)
  if(nr_DT_pre != nr_DT) {
    if(nr_DT == 0L) {
      stop("The treatment group does not have any rows with exclusively finite values.")
    }
    warning("Dropping ", nr_DT_pre - nr_DT, " treatment group rows because of non-finite values.")
  }
  nr_DT_pre <- nrow(r_control)
  r_control <- r_control[is.finite(r) & is.finite(d) & is.finite(unit_id),]
  nr_DT <- nrow(r_control)
  if(nr_DT_pre != nr_DT) {
    if(nr_DT == 0L) {
      stop("The control group does not have any rows with exclusively finite values.")
    }
    warning("Dropping ", nr_DT_pre - nr_DT, " control group rows because of non-finite values.")
  }

  # sort data with respect to id and time
  data.table::setorder(r_treat, unit_id, d)
  data.table::setorder(r_control, unit_id, d)

  #-----------------------------------------------------------------------------
  # setup data in required form

  # reshape treatment returns
  r_treat <- split(r_treat, by = "ed")
  if(ncores == 1L) {
    r_treat <- data.table::rbindlist(
      lapply(
        r_treat,
        get_set,
        estwind = estwind,
        eventwind = eventwind,
        estobs_min = estobs_min,
        eventobs_min = eventobs_min,
        eventdate = NULL
      )
    )
  } else {
    if(is_windows) {
      r_treat <- data.table::rbindlist(
        mirai::mirai_map(
          r_treat,
          get_set,
          .args = list(
            estwind = estwind,
            eventwind = eventwind,
            estobs_min = estobs_min,
            eventobs_min = eventobs_min,
            eventdate = NULL
          )
        )[]
      )
    } else {
      r_treat <- data.table::rbindlist(
        parallel::mclapply(
          r_treat,
          get_set,
          estwind = estwind,
          eventwind = eventwind,
          estobs_min = estobs_min,
          eventobs_min = eventobs_min,
          eventdate = NULL,
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
          get_set = get_set,
          .args = list(
            cdata = r_control,
            estwind = estwind,
            eventwind = eventwind,
            estobs_min = estobs_min,
            eventobs_min = eventobs_min
          )
        )[]
      )
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

  out <- list(
    r_treat,
    r_control
  )

  return(out)
}
