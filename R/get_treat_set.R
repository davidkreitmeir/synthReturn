NULL
###################################################################################
#' Function that pre-processes returns of the control group
#'
#' @description \code{get_treat_set} is used to create "event panels" for firms in the treatment group.
#'
#' @param eventdate date of event.
#' @param data The name of the data.table that contains the data.
#' @param estwind Argument to set estimation window period in relative time to event, i.e. `c(estwind_start, estwind_end)`
#' @param eventwind Argument to set event window period in relative time to event, i.e. `c(eventwind_start, eventwind_end)`
#' @param estobs_min Argument to define minimum number of trading days during the estimation window.
#' Can be an integer or a proportional (i.e. between 0 and 1). Default is \eqn{1}, i.e. no missing trading days
#' are allowed.
#' @param estobs_min Argument to define minimum number of trading days during the event window. Can be an
#' integer or a proportional (i.e. between 0 and 1). Default is \eqn{1}, i.e. no missing trading days are allowed.
#'
#' @return A data.table containing the following columns:
#'  \item{tid}{Unique (event \eqn{\times}) treatment firm identifier.}
#'  \item{d}{Trading Date.}
#'  \item{ed}{Event date}
#'  \item{r}{Stock return.}
#'  \item{estwind}{Indicator variable for estimation window. Equal to 1 if trading day during estimation window,
#'  and 0 otherwise.}
#'  \item{eventobs_min}{Indicator variable for event window. Equal to 1 if trading day during event window,
#'  and 0 otherwise.}
#'
#' @import data.table
#' @importFrom data.table .N
#' @importFrom data.table ':='
#' @importFrom purrr possibly


get_treat_set <- function(
  eventdate,
  tdata,
  estwind,
  eventwind,
  estobs_min,
  eventobs_min
){

  # print(eventdate)
  evdate <- eventdate

  # get relative time variable
  out <- tdata[ed == evdate,]
  out <- out[, datenum := 1:.N, by = tid]
  target <- out[, target := ifelse(d == ed, datenum, 0)][, .(target = base::max(target)), by = tid]
  out <- out[, target := NULL][target, on = .(tid)]
  out <- out[, tau := datenum - target][, `:=` (target = NULL,datenum = NULL)]
  # indicator for event and estimation window
  out <- out[, estwind := base::ifelse(tau >= estwind[1] & tau <= estwind[2], 1,0)][, eventwind := base::ifelse(tau >= eventwind[1] & tau <= eventwind[2], 1,0) ]
  out <- out[(estwind == 1 | eventwind == 1),]
  # get number of non-missing trading days for estimation AND event windows
  out <- out[, n_est_obs := base::sum(!is.na(r) & base::is.finite(r) & estwind == 1), by = tid]
  out <- out[, n_event_obs := base::sum(!is.na(r) & base::is.finite(r) & eventwind == 1), by = tid]
  # Drop thinly-traded firms and firms without return variance during entire sample period
  var_ids <- out[r != 0 & !is.na(r) & base::is.finite(r),][,.(r_var = .N), by = .(tid)][r_var > 0, "tid"]$tid
  tids <- out[(n_est_obs >= estobs_min) & (n_event_obs >= eventobs_min), .SD[1], by = "tid"]
  tids <- tids[tid %in% var_ids, c("tid", "ed")]
  tids <- tids$tid

  # return "event panels" of firms in treatment group for event d
  out <- out[tid %in% tids, c("tid", "d", "ed", "r","estwind","eventwind")]

  return(out)

}

# make sure code does not break because of an error during calculation of a specific corporation
get_treat_set =  purrr::possibly(get_treat_set, otherwise = NULL, quiet = TRUE)

