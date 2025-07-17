
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
  inference,
  correction,
  ncontrol_min,
  ndraws,
  ncores,
  static_scheduling,
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
    message("estwind incorporates time periods >= 0. Thus, it does not exclusively select control units based on pre-treatment time periods. This is ",
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
    message("The estwind interval does not strictly preceed the eventwind interval. Usually, you want the estimation window to refer only to time periods ",
      "before the event window.")
  }

  # make sure treatment indicator is logical
  if(!is.logical(DT[[treatname]])) {
    stop("data[, treatname] must be logical. Please convert it.")
  }

  # make sure the date variables are either of format date or numeric
  if(inherits(DT[[dname]], "Date")) {
    if(!inherits(DT[[edname]], "Date")) {
      stop("If data[, dname] is of format date, data[, dname] must be of format date as well.")
    }
  } else {
    # make sure dates are of format numeric
    if(!is.numeric(DT[[dname]])) {
      stop("data[, dname] must be of format date. Please convert it.")
    }
    # make sure edname is of format numeric
    if(!is.numeric(DT[[edname]])) {
      stop("data[, edname] must be of format dname. Please convert it.")
    }
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

  # check inference options
  if(inference != "none") {
    if(length(ndraws) != 1L || !is.numeric(ndraws) || !is.finite(ndraws)) {
      stop("ndraws must be numeric and of length one.")
    }
    if(ndraws < 1L || as.integer(ndraws) != ndraws){
      stop("The value of ndraws has to be an integer (>=) 1. Please convert it.")
    }
  }

  # Check control group size minimum
  if(length(ncontrol_min) != 1L || !is.numeric(ncontrol_min) || !is.finite(ncontrol_min)) {
    stop("ncontrol_min must be numeric and of length one.")
  }
  if(ncontrol_min <= 1L) {
    stop("The value of ncontrol_min has to be larger than 1. Please convert it.")
  }

  # if minimum trading days during estimation window in percent, convert to next (smallest) integer
  if(estobs_min %between% (0:1)) {
    estobs_min <- floor(estobs_min * estwindlen)
  }
  # if minimum trading days during event window in percent, convert to next (smallest) integer
  if(eventobs_min %between% (0:1)) {
    eventobs_min <- floor(eventobs_min * eventwindlen)
  }

  # Check scheduling choice
  if(length(static_scheduling) != 1L || !is.logical(static_scheduling) || is.na(static_scheduling)) {
    stop("static_scheduling must be either TRUE or FALSE.")
  }

  # Check inference correction choice
  if(inference == "none"){
    if(correction){
      message("correction only available for permutation and bootstrapping inference.")
    }
  } else {
    if(length(correction) != 1L || !is.logical(correction) || is.na(correction)) {
      stop("correction must be either TRUE or FALSE.")
    }
  }

  # Check if unit_id is unique by dname
  if(anyDuplicated(DT[, c(dname, unitname)]) != 0) {
    stop("The value of unitname must be the unique (by dname). I.e. a unit must not be observed more than once per time period.")
  }

  # Drop NA treat rows
  if(anyNA(DT[[treatname]])) {
    nr_DT_pre <- nrow(DT)
    DT <- na.omit(DT, cols = treatname)
    message("Dropping ", nr_DT_pre - nrow(DT), " rows because of NA values in ", treatname, ".")
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
  # unit variable will be denoted by unit_id
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
    message("Dropping ", nr_DT_pre - nr_DT, " treatment group rows because of non-finite values.")
  }
  nr_DT_pre <- nrow(r_control)
  r_control <- r_control[is.finite(r) & is.finite(d) & is.finite(unit_id),]
  nr_DT <- nrow(r_control)
  if(nr_DT_pre != nr_DT) {
    if(nr_DT == 0L) {
      stop("The control group does not have any rows with exclusively finite values.")
    }
    message("Dropping ", nr_DT_pre - nr_DT, " control group rows because of non-finite values.")
  }

  # sort data with respect to id and time
  data.table::setkey(r_treat, unit_id, d)
  data.table::setkey(r_control, unit_id, d)

  # Check if each unit has no more than 1 event date
  if(any(r_treat[, .(mult_ed = data.table::uniqueN(ed) != 1L), by = "unit_id"][["mult_ed"]])) {
    stop("Each unit must not have more than one event date. If a unit is treated multiple times, include it with a distinct unit ID per event date.")
  }

  #-----------------------------------------------------------------------------
  # setup data in required form

  # reshape treatment returns
  r_treat_ed <- r_treat[, .(ed = ed[1L]), by = "unit_id"][, "ed"]
  r_treat[, ed := NULL]
  r_treat <- split(r_treat, by = "unit_id", keep.by = FALSE)
  n_treat_pre <- length(r_treat)
  if(ncores == 1L) {
    r_treat <- mapply(
      get_treat_set,
      eventdate = r_treat_ed[["ed"]],
      out = r_treat,
      MoreArgs = list(
        estwind = estwind,
        eventwind = eventwind,
        estobs_min = estobs_min,
        eventobs_min = eventobs_min
      ),
      SIMPLIFY = FALSE,
      USE.NAMES = FALSE
    )
  } else {
    if(is_windows) {
      cl <- mirai::make_cluster(ncores)
      r_treat <- parallel::clusterMap(
        cl,
        get_treat_set,
        eventdate = r_treat_ed[["ed"]],
        out = r_treat,
        MoreArgs = list(
          estwind = estwind,
          eventwind = eventwind,
          estobs_min = estobs_min,
          eventobs_min = eventobs_min
        ),
        SIMPLIFY = FALSE,
        USE.NAMES = FALSE
      )
      mirai::stop_cluster(cl)
    } else {
      r_treat <- parallel::mcmapply(
        get_treat_set,
        eventdate = r_treat_ed[["ed"]],
        out = r_treat,
        MoreArgs = list(
          estwind = estwind,
          eventwind = eventwind,
          estobs_min = estobs_min,
          eventobs_min = eventobs_min
        ),
        SIMPLIFY = FALSE,
        USE.NAMES = FALSE,
        mc.cores = ncores,
        mc.preschedule = static_scheduling
      )
    }
  }
  r_treat_null <- vapply(r_treat, is.null, logical(1L), USE.NAMES = FALSE)
  if(any(r_treat_null)) {
    if(all(r_treat_null)) {
      stop("No treatment unit has both sufficient observations in estimation or event windows and variance in returns.")
    }
    message("Dropping ", sum(r_treat_null, na.rm = TRUE), " treatment units because of insufficient observations in estimation or event windows or because",
      " of no variance in returns.")
    r_treat_null <- !r_treat_null
    r_treat <- r_treat[r_treat_null]
    r_treat_ed <- r_treat_ed[r_treat_null,] # selected treatment units' event dates
  }

  # reshape control returns
  eds <- unique(r_treat_ed)[["ed"]]
  not_permutation <- inference != "permutation"
  if(ncores == 1L) {
    r_control <- lapply(
      eds,
      get_control_set,
      cdata = r_control,
      estwind = estwind,
      eventwind = eventwind,
      estobs_min = estobs_min,
      eventobs_min = eventobs_min,
      ncontrol_min = ncontrol_min,
      not_permutation = not_permutation
    )
  } else {
    if(is_windows) {
      r_control <- mirai::mirai_map(
        eds,
        get_control_set,
        .args = list(
          cdata = r_control,
          estwind = estwind,
          eventwind = eventwind,
          estobs_min = estobs_min,
          eventobs_min = eventobs_min,
          ncontrol_min = ncontrol_min,
          not_permutation = not_permutation
        ),
        .compute = "synthReturn"
      )[]
    } else {
      r_control <- parallel::mclapply(
        eds,
        get_control_set,
        cdata = r_control,
        estwind = estwind,
        eventwind = eventwind,
        estobs_min = estobs_min,
        eventobs_min = eventobs_min,
        ncontrol_min = ncontrol_min,
        not_permutation = not_permutation,
        mc.cores = ncores,
        mc.preschedule = static_scheduling
      )
    }
  }
  r_control_null <- vapply(r_control, is.null, logical(1L), USE.NAMES = FALSE)
  if(any(r_control_null)) {
    if(all(r_control_null)) {
      stop("No event date has sufficient control units, according to ncontrol_min.")
    }
    message("Dropping ", sum(r_control_null, na.rm = TRUE), " event dates because of too few control units, according to ncontrol_min.")
    r_control_null <- !r_control_null
    r_control <- r_control[!r_control_null]
    eds <- eds[r_control_null]
    keep_eds <- r_treat_ed[.(eds), which = TRUE, on = "ed"]
    r_treat <- r_treat[keep_eds]
    r_treat_ed <- r_treat_ed[keep_eds,]
  }
  names(r_control) <- as.character(eds)

  out <- list(
    r_treat = r_treat,
    r_control = r_control,
    r_treat_ed = as.character(r_treat_ed[["ed"]]),
    n_treat_pre = n_treat_pre
  )

  return(out)
}
