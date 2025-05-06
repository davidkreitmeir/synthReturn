###################################################################################
#' Function that pre-processes returns of the control group
#'
#' @description \code{get_event_panel} is used to create final "event panel" data set. Control corporations
#' are chosen such that a balanced panel in relative event time is created.
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
#'  \item{cid}{Unique (event \eqn{\times}) control firm identifier.  Treated firm get the unassigned cid of 0.}
#'  \item{t}{Time variable (consecutive).}
#'  \item{estwind}{Estimation window indicator.}
#'  \item{eventwind}{window indicator.}
#'  \item{r}{Stock return.}
#'

event_panel <- function(dt_treat, dt_control){

  out <- tryCatch({
    # get set of all "potential" control companies for event date
    edate <- dt_treat[1L, "ed"][["ed"]]
    r_event <- dt_control[ed == edate,]
    ndt_treat <- nrow(dt_treat)

    # select control companies
    # (1) No missing trading days on days treated corporation is traded
    cids <- r_event[dt_treat[, "d"], "unit_id", nomatch = NULL, on = "d"][, .(tdays = .N), by = "unit_id"][tdays == ndt_treat, "unit_id"]
    # (2) price changes need to be observed during sample period
    cids <- r_event[cids, c("unit_id", "r"), nomatch = NULL, on = "unit_id"][, .(r_var = stats::var(r, na.rm = TRUE)), by = "unit_id"][r_var > 0, "unit_id"]

    # filter control corp set
    r_event <- r_event[cids, nomatch = NULL, on = "unit_id"][dt_treat[, "d"], c("unit_id", "r"), nomatch = NULL, on = "d"]
    rm(cids)
    data.table::setorder(r_event, "unit_id")
    # give treated firm the unique and not assigned unit_id = 0
    r_event <- rbind(dt_treat[, c("unit_id", "r")], r_event)
    r_event[1:ndt_treat, unit_id := 0L]
    r_event[, t := rep.int(1:ndt_treat, nrow(r_event) / ndt_treat)]

    out <- ar_comp(r_event)

    return(out)
  }, error = function(x) return(NULL))

  return(out)
}
