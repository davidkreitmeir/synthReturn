NULL
###################################################################################
#' Function that pre-processes returns of the control group
#'
#' @description \code{get_control_set} is used to pre-process the returns of the control group
#' and returns an event date specific set of "potential" control companies that fulfill the stated conditions
#'
#'
#' @param returns data frame containing returns for treatment and control group or list of
#' two data frames of the form `list(ret_treat, ret_control)` with the first data frame `ret_treat`
#' comprising the returns of the treatment group and the second data frame `ret_control`
#' containing the returns for the control group .
#' @param estwind event window interval `c(est_start, est_end)` in relative time to event date. E.g. `c(-280, -30)`
#' denotes an estimation window of 250 days ending 30 days prior to the event day.
#' @param eventwind event window interval `c(event_start, event_end)` in relative time to event date. E.g.
#' `c(0, 10)` denotes an event window of 10 days starting on the event day 0 and ending 10 days post the event day.
#' @return A data.table containing the following columns:
#'  \item{ATT}{The TWFE DID point estimate}
#'
#' @export

synthReturn <- function(
  returns,
  treatid,
  controlid,
  ret,
  date,
  eventdate,
  estwind,
  eventwind,
  estobs_min = 1,
  eventobs_min = 1,
  placebo = TRUE,
  K,
  ){

  # set key variables

  # check if `returns` is a list: if yes unpack

  # convert to data.table if not already

  # check if all variables needed are in data.table


  estwindlen = estwind[2] - estwind[1] + 1
  eventwindlen = eventwind[2] - eventwind[1] + 1
  # if minimum trading days during estimation window in percent
  if(estobs_min >= 0 & estobs_min <= 1){
    estobs_min = floor(estobs_min*estwindlen)
  }
  # if minimum trading days during event window in percent
  if(estobs_min >= 0 & estobs_min <= 1){
    eventobs_min = floor(eventobs_min*eventwindlen)
  }

  # get returns


  # run placebo test for inference
  if(placebo == TRUE){





  }



  }
