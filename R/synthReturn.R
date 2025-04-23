NULL
###################################################################################
#' Function that
#'
#' @description \code{synthReturn} computes the average treatment effect \eqn{\phi} using the synthetic
#'  matching method suggested by Acemoglu et al. (2016) and modified by Kreitmeir et al. (2023) to
#'  accommodate multiple event dates and missing values.
#'
#' @param data The name of the data.frame that contains the data. Data should be stored in the "long" format.
#' @param tidname The name of the column containing the treated unit id.
#' @param tidname The name of the column containing the control unit id.
#' @param dname The name of the column containing the date variable.
#' @param rname The name of the column containing the stock returns.
#' @param edname The name of the column containing the (treatment firm specific) event date.
#' @param estwind Argument to set estimation window period in relative time to event, i.e. `c(estwind_start, estwind_end)`
#' @param eventwind Argument to set event window period in relative time to event, i.e. `c(eventwind_start, eventwind_end)`
#' @param estobs_min Argument to define minimum number of trading days during the estimation window.
#' Can be an integer or a proportional (i.e. between 0 and 1). Default is \eqn{1}, i.e. no missing trading days
#' are allowed.
#' @param estobs_min Argument to define minimum number of trading days during the event window. Can be an
#' integer or a proportional (i.e. between 0 and 1). Default is \eqn{1}, i.e. no missing trading days are allowed.
#' @param placebo Whether inference via placebo treatment group effects should be drawn. Default is `TRUE`
#' @param ndraws Number of randomly drawn placebo treatment group at each (unique) event date. Has to be larger than \eqn{1}.
#' @param ngroup Minimum number of control firms in placebo event(-date) panel relative to placebo treatment group size.
#' Default is \eqn{2}, i.e. placebo control group size has to be at least as large as size of placebo treatment group.
#'
#' @return A list containing the following components:
#' \item{ate}{Data.frame containing the average treatment effect estimates \eqn{\phi} and (if `placebo == TRUE`) the 90%, 95% and 99% confidence intervals.}
#' \item{ar}{Data.frame containing the estimated abnormal returns, and the "goodness" of the synthetic match estimate \eqn{\sigma} for all firms in the (actual) treatment group.}
#' \item{placebo}{List containing the average treatment effect estimates \eqn{\phi} for each placebo treatment group and the number of placebo treatment groups.}
#' \item{argu}{Some arguments used in the call (estwind, eventwind, estobs_min, eventobs_min, ngroup, ndraws)}
#'
#' @importFrom data.table .N
#' @importFrom data.table :=
#' @importFrom data.table %between%
#' @importFrom infer rep_slice_sample
#' @importFrom lubridate is.Date
#'
#' @examples
#' # -----------------------------------------------
#' # Example with two event-dates and no missing values
#' # -----------------------------------------------
#'
#' # Load data in that comes in the synthReturn package
#' data(ret_two_evdates)
#'
#' # -----------------------------------------------
#' # Implement the synthetic matching matching method
#'
#' synthReturn(
#'    data = ret_two_evdates,
#'    tidname = "treatid",
#'    cidname = "controlid",
#'    rname = "ret",
#'    dname = "date",
#'    edname = "eventdate",
#'    estwind = c(-100,-1),
#'    eventwind = c(0,5),
#'    estobs_min = 1,
#'    eventobs_min = 1,
#'   placebo = TRUE,
#'   ngroup = 2,
#'   ndraws = 10
#'   )
#'
#'
#' # -----------------------------------------------
#' # Example with two event-dates and no missing values
#' # -----------------------------------------------
#'
#' # Load data in that comes in the synthReturn package
#' data(ret_two_evdates_na)
#'
#' # -----------------------------------------------
#' # Implement the synthetic matching matching method
#'
#' # Note: You can set a threshold for the minimum of non-missing trading days
#' # during both the estimation (estobs_min) and event window (eventobs_min). Here, a firm
#' # is required to have non-missing returns for at least 90% of trading days during both,
#' # the estimation and event window (Default is 1 = 100%).
#'
#' synthReturn(
#'    data = ret_two_evdates,
#'    tidname = "treatid",
#'    cidname = "controlid",
#'    rname = "ret",
#'    dname = "date",
#'    edname = "eventdate",
#'    estwind = c(-100,-1),
#'    eventwind = c(0,5),
#'    estobs_min = 0.9,
#'    eventobs_min = 0.9,
#'   placebo = TRUE,
#'   ngroup = 2,
#'   ndraws = 10
#'   )
#'


#' @export
synthReturn <- function(
  data,
  tidname,
  cidname,
  dname,
  rname,
  edname,
  estwind,
  eventwind,
  estobs_min = 1,
  eventobs_min = 1,
  placebo = TRUE,
  ngroup = 2,
  ndraws = 25
  ){


  #-----------------------------------------------------------------------------
  # Pre-process data
  dp <- pre_process_synthReturn(
    data = data,
    tidname = tidname,
    cidname = cidname,
    rname = rname,
    dname = dname,
    edname = edname,
    estwind = estwind,
    eventwind = eventwind,
    estobs_min = estobs_min,
    eventobs_min = eventobs_min,
    placebo = placebo,
    ngroup = ngroup,
    ndraws = ndraws
  )

  #-----------------------------------------------------------------------------
  # Implement the methods

  r_treat = dp[[1L]]
  r_control = dp[[2L]]

  # Compute average treatment effect `phi` (for "actual" treatment group)
  res <- phi_comp(
    r_treat = r_treat,
    r_control = r_control
  )

  #-----------------------------------------------------------------------------
  # Create confidence intervals from average treatment effects of placebo treatment group

  if(placebo){

    # unique event dates
    eds <- unique(r_treat[, .SD[1], by = tid]$ed)
    # number of treated corporations
    n_treat <- data.table::uniqueN(r_treat, by = "tid")

    cid_placebo <- r_control[, .SD[1], by = c("cid", "ed")]
    cid_placebo[, c("r", "d") := NULL]
    ed_placebo <- cid_placebo[, ntotal := 1:.N, by = "ed"]
    # restrict set of placebo event dates by minimum number of control firms in event(-date) panel
    ngroup_min <- floor(ngroup*n_treat)
    ed_placebo <- ed_placebo[, .(ntotal = max(ntotal)), by = "ed"][ntotal >= ngroup_min, "ed"]
    # final set of placebo event dates (and potentially placebo treated firms)
    cid_placebo <- cid_placebo[ed_placebo, c("cid", "ed"), nomatch = NULL, on = "ed"]
    rm(ed_placebo)

    # `ndraws` random draws of placebo treatment groups of size `n` (with replacement)
    # for each (unique) event date.
    pids <- split(cid_placebo, by = "ed")
    pids <- lapply(
      pids,
      infer::rep_slice_sample,
      n = n_treat,
      replace = TRUE,
      reps = ndraws
    )
    # generate unique placebo id (pid = ndraws x number_of_event_dates)
    pids <- data.table::rbindlist(pids, use.names = TRUE, idcol = "edid")
    pids <- convert_DT(pids)
    pids <- pids[, pid := .GRP, by = .(edid, replicate)]
    pids <- pids[, c("edid", "replicate") := NULL]
    pids <- split(pids, by = "pid")

    # compute treatment effect for all placebo treatment groups
    phi_placebo <- lapply(
      pids,
      phi_comp_placebo,
      dt_control = r_control,
      estwind = estwind
    )

    phi_placebo <- data.table::rbindlist(phi_placebo, use.names = TRUE, idcol = "pid")
    # get number of placebo treatment effects
    n_placebo <- data.table::uniqueN(phi_placebo, by = "pid")

    # calculate CI intervals
    phi_CI90 = phi_placebo[, .(ci_90_lower = stats::quantile(phi, probs = 0.05), ci_90_upper = stats::quantile(phi, probs = 0.95)), by = "tau"]
    phi_CI95 = phi_placebo[, .(ci_95_lower = stats::quantile(phi, probs = 0.025), ci_95_upper = stats::quantile(phi, probs = 0.975)), by = "tau"]
    phi_CI99 = phi_placebo[, .(ci_99_lower = stats::quantile(phi, probs = 0.005), ci_99_upper = stats::quantile(phi, probs = 0.995)), by = "tau"]

    # create "output" table
    restab <- res[["phi"]]
    restab <- restab[phi_CI90, on = "tau"][phi_CI95, on = "tau"][phi_CI99, on = "tau"]

    # return all information of interest
    out = list(ate = restab, ar = res[["ar"]], placebo = list(phi_placebo = phi_placebo, n_placebo = n_placebo))


  } else {

    # return all information of interest (no CIs)
    restab <- res[["phi"]]
    out <- list(ate = restab, ar = res[["ar"]], placebo = list(phi_placebo = NULL, n_placebo = NULL))

  }

  # record the call
  call.param <- match.call()
  # Record all arguments used in the function
  argu <- mget(names(formals()), sys.frame(sys.nframe()))
  argu <- list(
    estwind = argu$estwind,
    eventwind = argu$eventwind,
    estobs_min = argu$estobs_min,
    eventobs_min = argu$eventobs_min,
    ngroup = argu$ngroup,
    ndraws = argu$ndraws
  )

  out <- append(out, list(arg = argu))

  # Define a new class
  class(out) <- "synthReturn"

  return(out)

}

