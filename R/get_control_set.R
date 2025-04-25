NULL
###################################################################################
#' Function that pre-processes returns of the control group
#'
#' @description \code{get_treat_set} is used to create an "event panel" of potential control group
#' firms for each (unique) event date.
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
#'  \item{cid}{Unique (event \eqn{\times}) control firm identifier.}
#'  \item{d}{Trading Date.}
#'  \item{ed}{Event date}
#'  \item{r}{Stock return.}
#'

get_control_set <- function(
  eventdate,
  cdata,
  estwind,
  eventwind,
  estobs_min,
  eventobs_min
) {

  # make sure code does not break because of an error during calculation of a specific corporation
  out <- tryCatch({
    # gen relative time variable
    out <- data.table::copy(cdata)
    out <- out[, ed := eventdate]
    out <- out[, datenum := 1:.N, by = "cid"]
    out[, targetv := data.table::fifelse(d == ed, datenum, 0)]
    target <- out[, .(targetv = max(targetv)), by = "cid"]
    out[, targetv := NULL]
    out <- out[target, on = "cid"]
    out[, tau := datenum - targetv]
    out[, c("targetv", "datenum") := NULL]
    # create indicator for
    out[, c("est_wind", "event_wind") := list(as.integer(tau %between% estwind), as.integer(tau %between% eventwind))]
    out <- out[(est_wind == 1L | event_wind == 1L),]
    # get no of non-missing trading days for estimation AND event windows
    out <- out[, n_est_obs := sum(is.finite(r) & est_wind == 1), by = "cid"]
    out <- out[, n_event_obs := sum(is.finite(r) & event_wind == 1), by = "cid"]
    # Drop thinly-traded companies or corporations without return variance
    var_ids <- out[r != 0 & is.finite(r),][, .(r_var = stats::var(r, na.rm = TRUE)), by = "cid"][r_var > 0, "cid"]
    cids <- unique(out[(n_est_obs >= estobs_min) & (n_event_obs >= eventobs_min), "cid"])[var_ids, "cid", nomatch = NULL, on = "cid"]

    # return set of potential control corporations for event date
    out <- out[cids, c("cid", "ed", "d", "r"), nomatch = NULL, on = "cid"]
    return(out)
  }, error = function(x) return(NULL))

  return(out)
}
