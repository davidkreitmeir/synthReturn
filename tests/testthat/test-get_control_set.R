devtools::load_all()

test_that("1 event, no missing returns", {

  #-----------------------------------------------------------------------------
  # Get data in right shape

  # set function arguments
  estwind = c(-100,-1)
  eventwind = c(0,5)
  estobs_min = 1
  eventobs_min = 1

  DT <- ret_one_evdate
  # Return variable will be denoted by r
  colnames(DT)[colnames(DT) == "ret"] <- "r"
  # date variable will be denoted by d
  colnames(DT)[colnames(DT) == "date"] <- "d"
  # event date variable will be denoted by ed
  colnames(DT)[colnames(DT) == "eventdate"] <- "ed"
  # treatment group identifier will be denoted by tid
  colnames(DT)[colnames(DT) == "treatid"] <- "tid"
  # control group identifier will be denoted by cid
  colnames(DT)[colnames(DT) == "controlid"] <- "cid"

  # convert data into 2 DTs: r_treat & r_control
  r_treat <- DT[!is.na(tid), c("tid", "d", "ed", "r")]
  r_control <- DT[!is.na(cid), c("cid", "d", "r")]

  # sort data with respect to id and time
  r_treat <- data.table::setorder(r_treat, tid, d)
  r_control <- data.table::setorder(r_control, cid, d)

  # get all event dates
  eds <- base::unique(r_treat[, .SD[1], by = tid]$ed)

  #-----------------------------------------------------------------------------
  # Check pre-process function

  expect_error(
    purrr::map(
      eds,
      get_control_set,
      data = r_control,
      estwind = estwind,
      eventwind = eventwind,
      estobs_min = estobs_min,
      eventobs_min = eventobs_min
    ),
    NA
  )

})


test_that("2 events, no missing returns", {

  #-----------------------------------------------------------------------------
  # Get data in right shape

  # set function arguments
  estwind = c(-100,-1)
  eventwind = c(0,5)
  estobs_min = 1
  eventobs_min = 1

  DT <- ret_two_evdates
  # Return variable will be denoted by r
  colnames(DT)[colnames(DT) == "ret"] <- "r"
  # date variable will be denoted by d
  colnames(DT)[colnames(DT) == "date"] <- "d"
  # event date variable will be denoted by ed
  colnames(DT)[colnames(DT) == "eventdate"] <- "ed"
  # treatment group identifier will be denoted by tid
  colnames(DT)[colnames(DT) == "treatid"] <- "tid"
  # control group identifier will be denoted by cid
  colnames(DT)[colnames(DT) == "controlid"] <- "cid"

  # convert data into 2 DTs: r_treat & r_control
  r_treat <- DT[!is.na(tid), c("tid", "d", "ed", "r")]
  r_control <- DT[!is.na(cid), c("cid", "d", "r")]

  # sort data with respect to id and time
  r_treat <- data.table::setorder(r_treat, tid, d)
  r_control <- data.table::setorder(r_control, cid, d)

  # get all event dates
  eds <- base::unique(r_treat[, .SD[1], by = tid]$ed)

  #-----------------------------------------------------------------------------
  # Check pre-process function

  expect_error(
    purrr::map(
      eds,
      get_control_set,
      data = r_control,
      estwind = estwind,
      eventwind = eventwind,
      estobs_min = estobs_min,
      eventobs_min = eventobs_min
    ),
    NA
  )

})


test_that("2 events, missing returns", {

  #-----------------------------------------------------------------------------
  # Get data in right shape

  # set function arguments
  estwind = c(-100,-1)
  eventwind = c(0,5)
  estobs_min = 1
  eventobs_min = 1

  DT <- ret_two_evdates_na
  # Return variable will be denoted by r
  colnames(DT)[colnames(DT) == "ret"] <- "r"
  # date variable will be denoted by d
  colnames(DT)[colnames(DT) == "date"] <- "d"
  # event date variable will be denoted by ed
  colnames(DT)[colnames(DT) == "eventdate"] <- "ed"
  # treatment group identifier will be denoted by tid
  colnames(DT)[colnames(DT) == "treatid"] <- "tid"
  # control group identifier will be denoted by cid
  colnames(DT)[colnames(DT) == "controlid"] <- "cid"

  # convert data into 2 DTs: r_treat & r_control
  r_treat <- DT[!is.na(tid), c("tid", "d", "ed", "r")]
  r_control <- DT[!is.na(cid), c("cid", "d", "r")]

  # sort data with respect to id and time
  r_treat <- data.table::setorder(r_treat, tid, d)
  r_control <- data.table::setorder(r_control, cid, d)

  # get all event dates
  eds <- base::unique(r_treat[, .SD[1], by = tid]$ed)

  #-----------------------------------------------------------------------------
  # Check pre-process function

  expect_error(
    purrr::map(
      eds,
      get_control_set,
      data = r_control,
      estwind = estwind,
      eventwind = eventwind,
      estobs_min = estobs_min,
      eventobs_min = eventobs_min
    ),
    NA
  )

  # set minimum lower
  estobs_min = 80
  eventobs_min = 4
  expect_error(
    purrr::map(
      eds,
      get_control_set,
      data = r_control,
      estwind = estwind,
      eventwind = eventwind,
      estobs_min = estobs_min,
      eventobs_min = eventobs_min
    ),
    NA
  )

})






