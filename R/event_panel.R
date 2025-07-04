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

event_panel <- function(dt_treat, treat_ed, dt_control, estwind, eventwind) {

  # dt_treat: data table; columns: d, r; sorted by d; single unit_id
  # dt_control: list of ed-specific data tables; columns: unit_id, d, r; sorted by unit_id, d; list elements are named according to ed value

  out <- tryCatch({
    # get set of all "potential" control companies for event date
    if(is.null(treat_ed)) {
      # phi_comp_placebo case
      r_control <- dt_control
    } else {
      # phi_comp case
      r_control <- dt_control[[as.character(treat_ed)]]
    }

    ndt_treat <- nrow(dt_treat)

    # select control companies
    # (1) No missing trading days on days treated corporation is traded
    cids <- r_control[dt_treat[, "d"], "unit_id", nomatch = NULL, on = "d"][, .(tdays = .N), by = "unit_id"][tdays == ndt_treat, "unit_id"]
    # (2) price changes need to be observed during sample period
    cids <- r_control[cids, c("unit_id", "r"), nomatch = NULL, on = "unit_id"][, .(r_var = stats::var(r, na.rm = TRUE)), by = "unit_id"][
      r_var > 0, "unit_id"]

    # filter control corp set
    r_control <- r_control[cids, nomatch = NULL, on = "unit_id"][dt_treat[, "d"], c("unit_id", "r", "tau"), nomatch = NULL, on = "d"]
    rm(cids)
    # dt_treat[, d := NULL]
    data.table::setorder(r_control, unit_id)

    ARs <- ar_comp(dt_treat, r_control, estwind, eventwind)

    # compute cumulative abnormal returns (CARs) and sigma
    ARs[, c("car_wgted", "one_div_sigma") := list(cumsum(ar) / sigma, 1 / sigma)]
    # filter out CARs or sigma's that are infinite or missing (perfect prediction by synthetic returns)
    ARs <- ARs[is.finite(car_wgted) & is.finite(one_div_sigma),]

    # ARs: data table with rows in eventwind period; columns: ar, sigma, tau, car_wgted, one_div_sigma

    return(ARs)
  }, error = function(x) return(NULL))

  return(out)
}
