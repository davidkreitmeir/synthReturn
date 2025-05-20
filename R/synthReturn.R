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
#' @param dname The name of the column containing the date variable. The column must either be of type `Date` or numeric. See details.
#' @param rname The name of the column containing the stock returns.
#' @param edname The name of the column containing the (treatment unit-specific) event date. All event dates must also exist as dates in `dname`. The
#' column must either be of type `Date` or numeric. Event date values are ignored for control group units. See details.
#' @param estwind Argument to set estimation window period in relative time to event, i.e. `c(estwind_start, estwind_end)`. 0 is the event date. The
#' interval only considers observed days. See details.
#' @param eventwind Argument to set event window period in relative time to event, i.e. `c(eventwind_start, eventwind_end)`. 0 is the event date. The
#' interval only considers observed days. See details.
#' @param estobs_min Argument to define minimum number of trading days during the estimation window.
#' Can be an integer or a proportion (i.e. between 0 and 1). Default is \eqn{1}, i.e. no missing trading days are allowed.
#' @param estobs_min Argument to define minimum number of trading days during the event window. Can be an
#' integer or a proportion (i.e. between 0 and 1). Default is \eqn{1}, i.e. no missing trading days are allowed.
#' @param placebo Logical denoting whether inference via placebo treatment group effects should be drawn. Default is `TRUE`.
#' @param ndraws Number of randomly drawn placebo treatment groups at each (unique) event date. Has to be larger than \eqn{1}.
#' @param ngroup Minimum number of control firms in placebo event(-date) panel relative to placebo treatment group size.
#' Default is \eqn{2}, i.e. placebo control group size has to be at least as large as size of placebo treatment group.
#' @param ncores Number of CPU cores to use. `NULL` (the default) sets it to the number of available cores.
#' @param static_scheduling Logical setting the parallel scheduling type on unix-like operating systems. `TRUE` (default) implies static scheduling,
#' `FALSE` dynamic scheduling. This parameter does not change the output object. It only influences the speed of the function. The scheduling choice has no
#' effect on Windows machines or when `ncores = 1`.
#'
#' @details The data's `dname` and `edname` columns refer to dates. `dname` is the date that a row refers to. `edname` is the date when a unit was treated.
#' I.e., `edname` is constant across all rows per unit. And it is ignored for never treated units.
#'
#' The package uses the term "term" for consistency with the literature. Internally, it does not care what interval a time period refers to. It evaluates
#' units' sequences of distinct `Date` or numerical values in `dname` and `edname`, irrespective of whether the denote days, hours, etc.
#'
#' `estwind` and `eventwind` describe sections in these sequences. 0 is the treatment time. Hence, `c(-1, -100)` is a unit's 100 observations before
#' treatment. When `dname` and `edname` are in days and a specific unit is observed on 2 days per week, `c(-1, -100)` covers 50 weeks before treatment in
#' the case of that unit. In the financial data, that would be a company's 100 trading days before treatment.
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
  ncores = NULL,
  static_scheduling = TRUE
) {

  # Parallel setting
  if(is.null(ncores)) {
    ncores <- parallel::detectCores()
  } else if(length(ncores) != 1L || ncores < 1L) {
    stop("ncores must be either NULL or a positive integer")
  }
  is_windows <- .Platform$OS.type == "windows"
  if(is_windows && ncores != 1L) {
    mirai::daemons(ncores, .compute = "synthReturn")
    on.exit(mirai::daemons(0L, .compute = "synthReturn"))
    mirai::everywhere(require("data.table"), .compute = "synthReturn")
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
    static_scheduling = static_scheduling,
    is_windows = is_windows
  )

  # dp[["r_treat"]]: list of unit-specific data tables; columns: d, r, tau; sorted by d; list elements are not named
  # dp[["r_control"]]: list of ed-specific data tables; columns: unit_id, d, r, tau; sorted by unit_id, d; list elements are named according to ed value
  # dp[["r_treat_unit_ed"]]: vector of treat unit ids; same order is r_treat list elements

  #-----------------------------------------------------------------------------
  # Implement the methods

  # Compute average treatment effect `phi` (for "actual" treatment group)
  res <- phi_comp(
    r_treat = dp[["r_treat"]],
    r_control = dp[["r_control"]],
    r_treat_ed = dp[["r_treat_ed"]],
    estwind = estwind,
    eventwind = eventwind,
    ncores = ncores,
    static_scheduling = static_scheduling,
    is_windows = is_windows
  )

  #-----------------------------------------------------------------------------
  # Create confidence intervals from average treatment effects of placebo treatment group

  if(placebo) {

    # number of treated units
    n_treat <- length(dp[["r_treat"]])
    dp[["r_treat"]] <- NULL

    ngroup_min <- floor(ngroup*n_treat)

    # ndraws random draws of placebo treatment groups of size n (with replacement) for each (unique) event date
    if(ncores == 1L) {
      phi_placebo <- lapply(
        dp[["r_control"]],
        function(r_control_ed) {
          r_control_ed_units <- unique(r_control_ed[, "unit_id"])[["unit_id"]]
          # restrict set of placebo event dates by minimum number of control firms in event(-date) panel
          if(length(r_control_ed_units) < ngroup_min) {
            return(NULL)
          }
          phi_placebo_ed <- lapply(1:ndraws, function(draw) {
            placebo_treat_ids <- sample(r_control_ed_units, n_treat, TRUE) # control unit ids chosen as placebo treatment group
            phi_placebo_draw <- phi_comp_placebo(placebo_treat_ids = placebo_treat_ids, r_control_ed = r_control_ed, estwind = estwind,
              eventwind = eventwind)
            return(phi_placebo_draw)
          })
          n_results_placebo_ed <- sum(!vapply(phi_placebo_ed, is.null, logical(1L), USE.NAMES = FALSE), na.rm = TRUE)
          phi_placebo_ed <- data.table::rbindlist(phi_placebo_ed)
          return(list(n_results_placebo_ed = n_results_placebo_ed, phi_placebo_ed = phi_placebo_ed))
        }
      )
    } else {
      if(is_windows) {
        phi_placebo <- mirai::mirai_map(
          dp[["r_control"]],
          function(r_control_ed, ndraws, n_treat, ngroup_min, estwind, eventwind) {
            r_control_ed_units <- unique(r_control_ed[, "unit_id"])[["unit_id"]]
            # restrict set of placebo event dates by minimum number of control firms in event(-date) panel
            if(length(r_control_ed_units) < ngroup_min) {
              return(NULL)
            }
            phi_placebo_ed <- lapply(1:ndraws, function(draw) {
              placebo_treat_ids <- sample(r_control_ed_units, n_treat, TRUE) # control unit ids chosen as placebo treatment group
              phi_placebo_draw <- phi_comp_placebo(placebo_treat_ids = placebo_treat_ids, r_control_ed = r_control_ed, estwind = estwind,
                eventwind = eventwind)
              return(phi_placebo_draw)
            })
            n_results_placebo_ed <- sum(!vapply(phi_placebo_ed, is.null, logical(1L), USE.NAMES = FALSE), na.rm = TRUE)
            phi_placebo_ed <- data.table::rbindlist(phi_placebo_ed)
            return(list(n_results_placebo_ed = n_results_placebo_ed, phi_placebo_ed = phi_placebo_ed))
          },
          .args = list(
            ndraws = ndraws,
            n_treat = n_treat,
            ngroup_min = ngroup_min,
            estwind = estwind,
            eventwind = eventwind
          ),
          .compute = "synthReturn"
        )[]
      } else {
        phi_placebo <- parallel::mclapply(
          dp[["r_control"]],
          function(r_control_ed, ndraws, n_treat, ngroup_min, estwind, eventwind) {
            r_control_ed_units <- unique(r_control_ed[, "unit_id"])[["unit_id"]]
            # restrict set of placebo event dates by minimum number of control firms in event(-date) panel
            if(length(r_control_ed_units) < ngroup_min) {
              return(NULL)
            }
            phi_placebo_ed <- lapply(1:ndraws, function(draw) {
              placebo_treat_ids <- sample(r_control_ed_units, n_treat, TRUE) # control unit ids chosen as placebo treatment group
              phi_placebo_draw <- phi_comp_placebo(placebo_treat_ids = placebo_treat_ids, r_control_ed = r_control_ed, estwind = estwind,
                eventwind = eventwind)
              return(phi_placebo_draw)
            })
            n_results_placebo_ed <- sum(!vapply(phi_placebo_ed, is.null, logical(1L), USE.NAMES = FALSE), na.rm = TRUE)
            phi_placebo_ed <- data.table::rbindlist(phi_placebo_ed)
            return(list(n_results_placebo_ed = n_results_placebo_ed, phi_placebo_ed = phi_placebo_ed))
          },
          ndraws = ndraws,
          n_treat = n_treat,
          ngroup_min = ngroup_min,
          estwind = estwind,
          eventwind = eventwind,
          mc.cores = ncores,
          mc.preschedule = static_scheduling
        )
      }
    }
    rm(dp)

    if(is_windows && ncores != 1L) {
      mirai::daemons(0L, .compute = "synthReturn")
      on.exit()
    }

    # get number of placebo treatment effects
    n_placebo <- sum(vapply(phi_placebo, `[[`, integer(1L), "n_results_placebo_ed", USE.NAMES = FALSE), na.rm = TRUE)
    phi_placebo <- data.table::rbindlist(lapply(phi_placebo, `[[`, "phi_placebo_ed"))

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

