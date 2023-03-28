NULL
###################################################################################
#' Function that pre-processes returns of the control group
#'
#' @description \code{get_treat_set} is used to create an "event panel" of potential control group
#' firms for each (unique) event date.
#'
#' @param ed date of event.
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
#'  \item{cid}{Unique (event \eqn{\times}) control firm identifier.}
#'  \item{d}{Trading Date.}
#'  \item{ed}{Event date}
#'  \item{r}{Stock return.}
#'
#' @import data.table
#' @importFrom purrr possibly

get_control_set <- function(
  ed,
  data,
  estwind,
  eventwind,
  estobs_min,
  eventobs_min
){

  # gen relative time variable
  data <- data[, ed := ed]
  data <- data[, datenum := 1:.N, by = cid]
  target <- data[, target := base::ifelse(d == ed, datenum, 0)][, .(target = max(target)), by = cid]
  data <- data[, target := NULL][target, on = .(cid)]
  data <- data[, tau := datenum - target][, `:=` (target = NULL,datenum = NULL)]
  # create indicator for
  data <- data[, est_wind := base::ifelse(tau >= estwind[1] & tau < estwind[2], 1,0)][, event_wind := base::ifelse(tau >= eventwind[1] & tau <= eventwind[2], 1,0) ]
  data <- data[(est_wind == 1 | event_wind == 1),]
  # get no of non-missing trading days for estimation AND event windows
  data <- data[, n_est_obs := base::sum(!is.na(r) & base::is.finite(r) & est_wind == 1), by = cid]
  data <- data[, n_event_obs := base::sum(!is.na(r) & base::is.finite(r) & event_wind == 1), by = cid]
  # Drop thinly-traded companies or corporations without return variance
  var_ids <- data[r != 0 & !is.na(r) & base::is.finite(r),][,.(r_var = .N), by = .(cid)][r_var > 0, "cid"]$cid
  cids <- data[(n_est_obs >= estobs_min) & (n_event_obs >= eventobs_min), .SD[1], by = "cid"]
  cids <- cids[cid %in% var_ids, c("cid", "ed")]
  cids <- cids$cid

  # return set of potential control corporations for event date
  data <- data[cid %in% cids, c("cid", "ed", "d", "r")]

  return(data)

}

# make sure code does not break because of an error during calculation of a specific corporation
get_control_set =  purrr::possibly(get_control_set, otherwise = NULL, quiet = TRUE)

