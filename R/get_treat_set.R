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

get_treat_set <- function(
  out,
  estwind,
  eventwind,
  estobs_min,
  eventobs_min
) {

  # make sure code does not break because of an error during calculation of a specific corporation
  out <- tryCatch({
    # get relative time variable
    out <- out[, datenum := 1:.N, by = "tid"]
    out[, targetv := data.table::fifelse(d == ed, datenum, 0L)]
    target <- out[, .(targetv = max(targetv)), by = "tid"]
    out[, targetv := NULL]
    out <- out[target, on = "tid"]
    out[, tau := datenum - targetv]
    out[, c("targetv", "datenum") := NULL]
    # indicator for event and estimation window
    out[, c("estwind", "eventwind") := list(as.integer(tau %between% estwind), as.integer(tau %between% eventwind))]
    out <- out[estwind == 1L | eventwind == 1L,]
    # get number of non-missing trading days for estimation AND event windows
    out <- out[, n_est_obs := sum(is.finite(r) & estwind == 1L), by = "tid"]
    out <- out[, n_event_obs := sum(is.finite(r) & eventwind == 1L), by = "tid"]
    # Drop thinly-traded firms and firms without return variance during entire sample period
    var_ids <- out[r != 0 & is.finite(r),][, .(r_var = stats::var(r, na.rm = TRUE)), by = "tid"][r_var > 0, "tid"]
    tids <- unique(out[(n_est_obs >= estobs_min) & (n_event_obs >= eventobs_min), "tid"])[var_ids, "tid", nomatch = NULL, on = "tid"]

    # return "event panels" of firms in treatment group for event d
    out <- out[tids, c("tid", "d", "ed", "r", "estwind", "eventwind"), nomatch = NULL, on = "tid"]
    return(out)
  }, error = function(x) return(NULL))

  return(out)
}
