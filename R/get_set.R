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

get_set <- function(
  out,
  estwind,
  eventwind,
  estobs_min,
  eventobs_min,
  eventdate
) {

  # make sure code does not break because of an error during calculation of a specific corporation
  out <- tryCatch({
    is_treat <- is.null(eventdate)
    # set event date
    if(is_treat) {
      eventdate <- out[1L, "ed"][["ed"]]
    }
    # get relative time variable
    out[, tau := (1:.N) - which(d == eventdate), by = "unit_id"]
    # drop units not observed on event date
    out <- na.omit(out, cols = "tau")
    # subset to observations in event and estimation windows
    out <- out[(tau %between% estwind) | (tau %between% eventwind),]
    # subset to units with enough observations in estimation window
    keep_units <- out[tau %between% estwind, .(n_est_obs = .N), by = "unit_id"][n_est_obs >= estobs_min, "unit_id"]
    out <- out[keep_units, nomatch = NULL, on = "unit_id"]
    # subset to units with enough observations in event window
    keep_units <- out[tau %between% eventwind, .(n_event_obs = .N), by = "unit_id"][n_event_obs >= eventobs_min, "unit_id"]
    out[, tau := NULL]
    out <- out[keep_units, nomatch = NULL, on = "unit_id"]
    # subset to units with return variance during entire sample period
    keep_units <- out[, .(r_var = stats::var(r, na.rm = TRUE)), by = "unit_id"][r_var > 0, "unit_id"]
    out <- out[keep_units, nomatch = NULL, on = "unit_id"]
    # set the event date in control group
    if(!is_treat) {
      out[, ed := eventdate]
    }

    # return "event panels" of units in treatment group for event d
    return(out)
  }, error = function(x) return(NULL))

  return(out)
}
