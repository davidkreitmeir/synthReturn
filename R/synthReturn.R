###################################################################################
#' Function that
#'
#' @description \code{synthReturn} computes the average treatment effect \eqn{\phi} using the synthetic
#'  matching method suggested by Acemoglu et al. (2016) and modified by Kreitmeir et al. (2023) to
#'  accommodate multiple event dates and missing values.
#'
#' @param data The name of the data frame that contains the data.
#' @param unitname The name of the column containing IDs of treated and control units.
#' @param treatname The name of the indicator column set to `TRUE` for the treatment group and to `FALSE` for the control group. The column's values must
#' be time-constant within a unit because they mark whether a unit was ever treated.
#' @param dname The name of the column containing the date variable. The column must either be of type `Date` or numeric. `Date` is internally converted to
#' numeric. See details.
#' @param rname The name of the column containing the stock returns.
#' @param edname The name of the column containing the (treatment unit-specific) event date. All event dates must also exist as dates in `dname`. The
#' column must either be of type `Date` or numeric. `Date` is internally converted to numeric. Event date values are ignored for control group units. See
#' details.
#' @param estwind Argument to set estimation window period in relative time to event, i.e. `c(estwind_start, estwind_end)`. 0 is the event date. See
#' details.
#' @param eventwind Argument to set event window period in relative time to event, i.e. `c(eventwind_start, eventwind_end)`. 0 is the event date. See
#' details.
#' @param estobs_min Argument to define minimum number of trading days during the estimation window.
#' Can be an integer or a proportion (i.e. between 0 and 1). Default is \eqn{1}, i.e. no missing trading days are allowed.
#' @param estobs_min Argument to define minimum number of trading days during the event window. Can be an
#' integer or a proportion (i.e. between 0 and 1). Default is \eqn{1}, i.e. no missing trading days are allowed.
#' @param placebo Logical denoting whether inference via placebo treatment group effects should be drawn. Default is `TRUE`.
#' @param ndraws Number of randomly drawn placebo treatment groups at each (unique) event date. Has to be larger than \eqn{1}.
#' @param ngroup Minimum number of control firms in placebo event(-date) panel relative to placebo treatment group size.
#' Default is \eqn{2}, i.e. placebo control group size has to be at least as large as size of placebo treatment group.
#' @param ncores Number of CPU cores to use. `NULL` (the default) sets it to the number of available cores.
#'
#' @details If the `dname` column is of `Date` type, the function creates a numeric representation by enumerating the dates existing in the data. I.e., it
#' creates
#'
#' |     dname_date | | dname_num|
#' |---------------:|-|---------:|
#' |12/12/2024      | |         1|
#' |13/12/2024      | |         2|
#' |16/12/2024      | |         3|
#' |17/12/2024      | |         4|
#'
#' and NOT
#'
#' |     dname_date | | dname_num|
#' |---------------:|-|---------:|
#' |12/12/2024      | |         1|
#' |13/12/2024      | |         2|
#' |16/12/2024      | |         5|
#' |17/12/2024      | |         6|
#'
#' It only counts observed days because this is the desired behavior in many use cases. Financial researchers, e.g., often only take trading days into
#' account and understand Monday as the next day after the previous Friday, since there is no stock trading on weekends.
#'
#' `estwind` and `eventwind` refer to those numeric values. Hence, in case a unit is treated on 16/12/2024, that date is day 0 in `estwind` and
#' `eventwind`. And the 13/12/2024 is day -1.
#'
#' If you want unobserved days to be counted, do not pass a column of `Date` type. Directly use a numeric column.
#'
#' @return An S3 object containing the following components:
#' \item{ate}{Data.frame containing the average treatment effect estimates \eqn{\phi} and (if `placebo == TRUE`) the 90%, 95% and 99% confidence intervals.}
#' \item{ar}{Data.frame containing the estimated abnormal returns, and the "goodness" of the synthetic match estimate \eqn{\sigma} for all firms in the
#' (actual) treatment group.}
#' \item{placebo}{List containing the average treatment effect estimates \eqn{\phi} for each placebo treatment group and the number of placebo treatment
#' groups.}
#' \item{argu}{Some arguments used in the call (estwind, eventwind, estobs_min, eventobs_min, ngroup, ndraws).}
#'
#' @importFrom data.table .N
#' @importFrom data.table .SD
#' @importFrom data.table .GRP
#' @importFrom data.table :=
#' @importFrom data.table %chin%
#' @importFrom data.table %between%
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
#'   data = ret_two_evdates,
#'   tidname = "treatid",
#'   cidname = "controlid",
#'   rname = "ret",
#'   dname = "date",
#'   edname = "eventdate",
#'   estwind = c(-100,-1),
#'   eventwind = c(0,5),
#'   estobs_min = 1,
#'   eventobs_min = 1,
#'   placebo = TRUE,
#'   ngroup = 2,
#'   ndraws = 10
#' )
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
#'   data = ret_two_evdates,
#'   tidname = "treatid",
#'   cidname = "controlid",
#'   rname = "ret",
#'   dname = "date",
#'   edname = "eventdate",
#'   estwind = c(-100,-1),
#'   eventwind = c(0,5),
#'   estobs_min = 0.9,
#'   eventobs_min = 0.9,
#'   placebo = TRUE,
#'   ngroup = 2,
#'   ndraws = 10
#' )
#'

#' @export
synthReturn <- function(
  data,
  unitname,
  treatname,
  dname,
  rname,
  edname,
  estwind,
  eventwind,
  estobs_min = 1,
  eventobs_min = 1,
  placebo = TRUE,
  ngroup = 2,
  ndraws = 25,
  ncores = NULL
) {

  # Parallel setting
  if(is.null(ncores)) {
    ncores <- parallel::detectCores()
  } else if(length(ncores) != 1L || ncores < 1L) {
    stop("ncores must be either NULL or a positive integer")
  }
  is_windows <- .Platform$OS.type == "windows"
  if(is_windows && ncores != 1L) {
    mirai::daemons(ncores)
    mirai::everywhere(require("data.table"))
  }

  #-----------------------------------------------------------------------------
  # Pre-process data
  dp <- pre_process_synthReturn(
    DT = data,
    unitname = unitname,
    treatname = treatname,
    rname = rname,
    dname = dname,
    edname = edname,
    estwind = estwind,
    eventwind = eventwind,
    estobs_min = estobs_min,
    eventobs_min = eventobs_min,
    placebo = placebo,
    ngroup = ngroup,
    ndraws = ndraws,
    ncores = ncores,
    is_windows = is_windows
  )

  #-----------------------------------------------------------------------------
  # Implement the methods

  r_treat <- dp[[1L]]
  r_control <- dp[[2L]]
  rm(dp)

  # Compute average treatment effect `phi` (for "actual" treatment group)
  res <- phi_comp(
    r_treat = r_treat,
    r_control = r_control
  )

  #-----------------------------------------------------------------------------
  # Create confidence intervals from average treatment effects of placebo treatment group

  if(placebo) {

    # number of treated units
    n_treat <- data.table::uniqueN(r_treat, by = "unit_id")

    cid_placebo <- unique(r_control[, c("unit_id", "ed")])
    # restrict set of placebo event dates by minimum number of control firms in event(-date) panel
    ngroup_min <- floor(ngroup*n_treat)
    ed_placebo <- cid_placebo[, .(ntotal = .N), by = "ed"][ntotal >= ngroup_min, "ed"]
    # final set of placebo event dates (and potentially placebo treated units)
    cid_placebo <- cid_placebo[ed_placebo, nomatch = NULL, on = "ed"]
    rm(ed_placebo)

    # `ndraws` random draws of placebo treatment groups of size `n` (with replacement) for each (unique) event date
    if(is_windows || ncores == 1L) {
      pids <- data.table::rbindlist(
        lapply(
          split(cid_placebo, by = "ed"),
          function(dt, n_treat, ndraws) {
            sampled_dt <- dt[as.vector(vapply(1:ndraws, sample.int, integer(n_treat), n = nrow(dt), size = n_treat, replace = TRUE)),]
            sampled_dt[, replicate := rep(1:ndraws, each = n_treat)]
            return(sampled_dt)
          },
          n_treat = n_treat,
          ndraws = ndraws
        ),
        use.names = TRUE,
        idcol = "edid"
      )
    } else {
      pids <- data.table::rbindlist(
        parallel::mclapply(
          split(cid_placebo, by = "ed"),
          function(dt, n_treat, ndraws) {
            sampled_dt <- dt[as.vector(vapply(1:ndraws, sample.int, integer(n_treat), n = nrow(dt), size = n_treat, replace = TRUE)),]
            sampled_dt[, replicate := rep(1:ndraws, each = n_treat)]
            return(sampled_dt)
          },
          n_treat = n_treat,
          ndraws = ndraws,
          mc.cores = ncores
        ),
        use.names = TRUE,
        idcol = "edid"
      )
    }
    # generate unique placebo id (pid = ndraws x number_of_event_dates)
    pids <- pids[, pid := .GRP, by = c("edid", "replicate")]
    pids <- pids[, c("edid", "replicate") := NULL]
    pids <- split(pids, by = "pid")

    # compute treatment effect for all placebo treatment groups
    phi_placebo <- data.table::rbindlist(
      lapply(
        pids,
        phi_comp_placebo,
        dt_control = r_control,
        estwind = estwind
      ),
      use.names = TRUE,
      idcol = "pid"
    )
    rm(pids)

    if(is_windows && ncores != 1L) {
      mirai::daemons(0L)
    }

    # get number of placebo treatment effects
    n_placebo <- data.table::uniqueN(phi_placebo, by = "pid")

    # calculate CI intervals
    phi_CI90 = phi_placebo[, .(ci_90_lower = stats::quantile(phi, probs = 0.05), ci_90_upper = stats::quantile(phi, probs = 0.95)), by = "tau"]
    phi_CI95 = phi_placebo[, .(ci_95_lower = stats::quantile(phi, probs = 0.025), ci_95_upper = stats::quantile(phi, probs = 0.975)), by = "tau"]
    phi_CI99 = phi_placebo[, .(ci_99_lower = stats::quantile(phi, probs = 0.005), ci_99_upper = stats::quantile(phi, probs = 0.995)), by = "tau"]

    # return all information of interest
    out <- list(
      ate = res[["phi"]][phi_CI90, on = "tau"][phi_CI95, on = "tau"][phi_CI99, on = "tau"],
      ar = res[["ar"]],
      placebo = list(phi_placebo = phi_placebo, n_placebo = n_placebo)
    )

  } else {

    # return all information of interest (no CIs)
    out <- list(ate = res[["phi"]], ar = res[["ar"]], placebo = list(phi_placebo = NULL, n_placebo = NULL))
  }

  # record the call
  call.param <- match.call()
  # Record all arguments used in the function
  argu <- mget(names(formals()), sys.frame(sys.nframe()))

  out[["arg"]] <- list(
    estwind = argu$estwind,
    eventwind = argu$eventwind,
    estobs_min = argu$estobs_min,
    eventobs_min = argu$eventobs_min,
    ngroup = argu$ngroup,
    ndraws = argu$ndraws
  )

  # Define a new class
  class(out) <- "synthReturn"

  return(out)
}

