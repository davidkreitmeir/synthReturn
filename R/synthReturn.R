###################################################################################
#' Function that
#'
#' @description \code{synthReturn} computes the average treatment effect \eqn{\phi} using the synthetic
#'  matching method suggested by Acemoglu et al. (2016) and revised by Kreitmeir et al. (2025).
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
#' @param inference Argument to define which inference method is to be used. Both permutation and bootstrap inference are implemented. Default is `"none"`.
#' @param correction Logical defining if "corrected" synthetic matching results are used for inference. If `TRUE` firms that do not have a good synthetic
#' match, defined as firms in the control group with \eqn{\sigma} more than \eqn{\sqrt3} times the average \eqn{\sigma} of the treated firms.
#' Default is `FALSE`.
#' @param ndraws Number of randomly drawn placebo treatment groups if `inference = "permutation"`. Number of nonparametric bootstrap repetitions if `inference = "bootstrap"`.
#' Has to be larger than \eqn{1}. `ndraws` has no effect if `inference = "none"`
#' @param ncontrol_min Minimum number of control firms required to create synthetic match. Default is \eqn{10}.
#' @param ncores Number of CPU cores to use. `NULL` (the default) sets it to the number of available cores.
#' @param static_scheduling Logical setting the parallel scheduling type. `TRUE` (default) implies static scheduling, `FALSE` dynamic scheduling. This
#' parameter does not change the output object. It only influences the speed of the function. The scheduling choice has no effect when `ncores = 1` and in
#' placebo estimations on Windows machines.
#'
#' @details The data's `dname` and `edname` columns refer to dates. `dname` is the date that a row refers to. `edname` is the date when a unit was treated.
#' I.e., `edname` is constant across all rows per unit. And it is ignored for never treated units.
#'
#' The package uses the term "term" for consistency with the literature. Internally, it does not care what interval a time period refers to. It evaluates
#' units' sequences of distinct `Date` or numerical values in `dname` and `edname`, irrespective of whether they denote days, hours, etc.
#'
#' `estwind` and `eventwind` describe sections in these sequences. 0 is the treatment time. Hence, `c(-1, -100)` is a unit's 100 observations before
#' treatment. When `dname` and `edname` are in days and a specific unit is observed on 2 days per week, `c(-1, -100)` covers 50 weeks before treatment in
#' the case of that unit. In the case of financial data, that would be a company's 100 trading days before treatment.
#'
#' @return An S3 object containing the following components:
#' \item{n_treat_pre}{Number of treatment units in the data.}
#' \item{n_treat_res}{Number of treatment units in the data that fulfill the minimum requirements and are used in the calculation of
#' the average treatment effect \eqn{\phi}.}
#' \item{ate}{Data.table containing the relative time period \eqn{\tau} and the average treatment effect estimates \eqn{\phi}. If the user selects
#' `inference = "permutation"`, the data.table additionally reports the p-value and the 95\% confidence interval. If the user selects
#' `inference = "bootstrap"`, the data.table additionally reports the standard error, the p-value and the 95 percent confidence interval.}
#' \item{ar}{Data.table reporting the estimated abnormal returns, the "goodness" of the synthetic match estimate \eqn{\sigma}, the weighted cumulative abnormal
#' return and the corresponding weights for all treated firms.}
#' \item{ate_bootstrap}{Data.table containing the average treatment effect estimates \eqn{\phi} for each bootstrap iteration. Returned
#' if the user chooses `inference = "bootstrap"`.}
#' \item{n_bootstrap}{Number of bootstrap iterations that returned a valid result.}
#' \item{ate_placebo}{Data.table containing the average treatment effect estimates \eqn{\phi} for each placebo treatment group draw. Returned
#' if the user chooses `inference = "permutation"`.}
#' \item{n_placebo}{Number of placebo draws that returned a valid result.}
#' \item{arg}{List with arguments used in the call.}
#'
#' @importFrom data.table .N
#' @importFrom data.table .SD
#' @importFrom data.table .GRP
#' @importFrom data.table :=
#' @importFrom data.table %chin%
#' @importFrom data.table %between%
#'
#' @examples
#'
#' # Load data in that comes in the synthReturn package
#' data(ret_two_evdates)
#'
#' # -----------------------------------------------
#' # Example with Permutation Inference
#' # -----------------------------------------------
#'
#' set.seed(123) # set random seed
#'
#' # Run synthReturn
#' res.placebo <- synthReturn(
#'   data = ret_two_evdates,
#'   unitname = "unit",
#'   treatname = "treat",
#'   dname = "date",
#'   rname = "ret",
#'   edname = "eventdate",
#'   estwind = c(-100,-1),
#'   eventwind = c(0,5),
#'   estobs_min = 1,
#'   eventobs_min = 1,
#'   inference = "permutation",
#'   correction = FALSE,
#'   ncontrol_min = 10,
#'   ndraws = 100,
#'   ncores = 1
#'   )
#'
#' # -----------------------------------------------
#' # Example with Nonparametric Bootstrap
#' # -----------------------------------------------
#'
#' set.seed(123) # set random seed
#'
#' # Run synthReturn
#' res.boot <- synthReturn(
#'   data = ret_two_evdates,
#'   unitname = "unit",
#'   treatname = "treat",
#'   dname = "date",
#'   rname = "ret",
#'   edname = "eventdate",
#'   estwind = c(-100,-1),
#'   eventwind = c(0,5),
#'   estobs_min = 1,
#'   eventobs_min = 1,
#'   inference = "bootstrap",
#'   correction = FALSE,
#'   ncontrol_min = 10,
#'   ndraws = 100,
#'   ncores = 1
#'   )


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
  inference = c("none", "permutation", "bootstrap"),
  correction = FALSE,
  ncontrol_min = 10, # absolute minimum number of control units per event date
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
  }

  #-----------------------------------------------------------------------------
  # Pre-process data
  # make sure inference is "none", "permutation", or "bootstrap"
  inference <- match.arg(inference)

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
    inference = inference,
    correction = correction,
    ncontrol_min = ncontrol_min,
    ndraws = ndraws,
    ncores = ncores,
    static_scheduling = static_scheduling,
    is_windows = is_windows
  )

  # dp[["r_treat"]]: list of unit-specific data tables; columns: d, r, tau; sorted by d; list elements are not named
  # dp[["r_control"]]: list of ed-specific data tables; columns: unit_id, d, r, tau; sorted by unit_id, d; list elements are named according to ed value
  # dp[["r_treat_ed"]]: character vector of treat units' event dates; same order is r_treat list elements
  # dp[["n_treat_pre"]]: number of treatment units with finite values in all relevant variables

  out <- list(n_treat_pre = dp[["n_treat_pre"]])
  dp[["n_treat_pre"]] <- NULL

  if(is_windows && ncores != 1L) {
    if(inference != "permutation") {
      if(inference == "none") {
        dp[["r_control"]] <- dp[["r_control"]][dp[["r_treat_ed"]]]
        dp[["r_treat_ed"]] <- NULL
      }
      mirai::daemons(0L, .compute = "synthReturn")
      on.exit()
    }
  }

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
    is_windows = is_windows,
    inference = inference
  )
  out[["n_treat_res"]] <- res[["n_treat_res"]]

  #-----------------------------------------------------------------------------
  # Create confidence intervals from average treatment effects of placebo treatment group

  if(inference == "none") {
    # return all information of interest (no CIs)
    out[["ate"]] <- res[["phi"]]
    out[["ar"]] <- res[["ar"]]

  } else {

    if(correction){
      sigma_cutoff <- sqrt(3) * mean(res[["ar"]][tau == 0L, "sigma"][["sigma"]], na.rm = TRUE)
    } else {
      sigma_cutoff <- NULL
    }

    is_single_core <- ncores == 1L

    if(inference == "permutation") {

      dp[["r_treat"]] <- NULL
      dp <- dp[["r_control"]][dp[["r_treat_ed"]]]

      # ndraws random draws of placebo treatment groups of size n (with replacement) for each (unique) event date
      phi_placebo <- lapply(1:ndraws, function(draw) {
        if(is_single_core) {
          draw_phi <- lapply(
            dp,
            function(r_control_ed) {
              r_control_ed_units <- unique(r_control_ed[, "unit_id"])[["unit_id"]]
              # restrict set of placebo event dates to those with at least two control companies
              if(length(r_control_ed_units) == 1L) {
                return(NULL)
              }
              placebo_treat_id <- sample(r_control_ed_units, 1L) # control unit ids chosen as placebo treatment group
              ars <- phi_comp_placebo(placebo_treat_id = placebo_treat_id, r_control_ed = r_control_ed, estwind = estwind, eventwind = eventwind,
                sigma_cutoff = sigma_cutoff)
              return(ars)
            }
          )
        } else {
          if(is_windows) {
            draw_phi <- mirai::mirai_map(
              dp,
              function(r_control_ed, estwind, eventwind, sigma_cutoff) {
                r_control_ed_units <- unique(r_control_ed[, "unit_id"])[["unit_id"]]
                # restrict set of placebo event dates to those with at least two control companies
                if(length(r_control_ed_units) == 1L) {
                  return(NULL)
                }
                placebo_treat_id <- sample(r_control_ed_units, 1L) # control unit ids chosen as placebo treatment group
                ars <- phi_comp_placebo(placebo_treat_id = placebo_treat_id, r_control_ed = r_control_ed, estwind = estwind, eventwind = eventwind,
                  sigma_cutoff = sigma_cutoff)
                return(ars)
              },
              .args = list(
                estwind = estwind,
                eventwind = eventwind,
                sigma_cutoff = sigma_cutoff
              ),
              .compute = "synthReturn"
            )[]
          } else {
            draw_phi <- parallel::mclapply(
              dp,
              function(r_control_ed, estwind, eventwind, sigma_cutoff) {
                r_control_ed_units <- unique(r_control_ed[, "unit_id"])[["unit_id"]]
                # restrict set of placebo event dates to those with at least two control companies
                if(length(r_control_ed_units) == 1L) {
                  return(NULL)
                }
                placebo_treat_id <- sample(r_control_ed_units, 1L) # control unit ids chosen as placebo treatment group
                ars <- phi_comp_placebo(placebo_treat_id = placebo_treat_id, r_control_ed = r_control_ed, estwind = estwind, eventwind = eventwind,
                  sigma_cutoff = sigma_cutoff)
                return(ars)
              },
              estwind = estwind,
              eventwind = eventwind,
              sigma_cutoff = sigma_cutoff,
              mc.cores = ncores,
              mc.preschedule = static_scheduling
            )
          }
        }
        n_results_placebo_draw <- sum(!vapply(draw_phi, is.null, logical(1L), USE.NAMES = FALSE), na.rm = TRUE)
        if(n_results_placebo_draw == 0L) {
          return(list(n_results_placebo_draw = 0L, phi_placebo_draw = NULL))
        }
        draw_phi <- data.table::rbindlist(draw_phi)
        # compute phi - equ. (7)
        draw_phi <- draw_phi[, .(phi = sum(car_wgted) / sum(one_div_sigma)), by = "tau"]
        return(list(n_results_placebo_draw = n_results_placebo_draw, phi_placebo_draw = draw_phi))
      })
      rm(dp)

      # get number of placebo treatment effects
      n_placebo <- sum(vapply(phi_placebo, `[[`, integer(1L), "n_results_placebo_draw", USE.NAMES = FALSE), na.rm = TRUE)
      if(n_placebo == 0L) {
        stop("permutation did not produce any results.")
      }
      phi_placebo <- data.table::rbindlist(lapply(phi_placebo, `[[`, "phi_placebo_draw"))
      data.table::setnames(phi_placebo, "phi", "phi_permut")

      # calculate (two-sided) p-value
      pval_placebo <- res[["phi"]][phi_placebo, c("tau", "phi", "phi_permut"), on = "tau"]
      pval_placebo[, `:=` (pval_lt = as.integer(phi < -abs(phi)), pval_ut = as.integer(phi > abs(phi)))]
      pval_placebo <- pval_placebo[, .(pval = mean(pval_lt, na.rm = TRUE) + mean(pval_ut, na.rm = TRUE)), by = "tau"]

      # calculate CI intervals
      # phi_CI90 <- phi_placebo[, .(ci_90_lower = stats::quantile(phi, probs = 0.05), ci_90_upper = stats::quantile(phi, probs = 0.95)), by = "tau"]
      phi_CI95 <- phi_placebo[, .(ci_95_lower = stats::quantile(phi_permut, probs = 0.025), ci_95_upper = stats::quantile(phi_permut, probs = 0.975)),
        by = "tau"]
      # phi_CI99 <- phi_placebo[, .(ci_99_lower = stats::quantile(phi, probs = 0.005), ci_99_upper = stats::quantile(phi, probs = 0.995)), by = "tau"]

      # return all information of interest
      out[["ate"]] <- res[["phi"]][pval_placebo, on = "tau"][phi_CI95, on = "tau"]
      out[["ar"]] <- res[["ar"]]
      out[["ate_placebo"]] <- phi_placebo
      out[["n_placebo"]] <- n_placebo

    } else if(inference == "bootstrap") {
      # number of treated units
      n_treat <- length(dp[["r_treat"]])

      if((!is_single_core) && is_windows) {
        scheduling <- data.table::fifelse(static_scheduling, "static", "dynamic")
        cl <- mirai::make_cluster(ncores)
      }

      phi_bootstrap <- lapply(1:ndraws, function(draw) {
        # sample treatment units
        treat_sample <- sample.int(n_treat, n_treat, TRUE)
        r_treat_sample <- dp[["r_treat"]][treat_sample]
        r_treat_sample_ed <- dp[["r_treat_ed"]][treat_sample]
        rm(treat_sample)
        if(is_single_core) {
          phi_bootstrap_draw <- mapply(
            function(r_treat_sample, r_treat_sample_ed, r_control, estwind, eventwind, sigma_cutoff) {
              return(phi_comp_bootstrap(r_treat_sample, r_control[[r_treat_sample_ed]], estwind, eventwind, sigma_cutoff))
            },
            r_treat_sample = r_treat_sample,
            r_treat_sample_ed = r_treat_sample_ed,
            MoreArgs = list(
              r_control = dp[["r_control"]],
              estwind = estwind,
              eventwind = eventwind,
              sigma_cutoff = sigma_cutoff
            ),
            SIMPLIFY = FALSE,
            USE.NAMES = FALSE
          )
          rm(r_treat_sample_ed)
        } else {
          r_control_sample <- dp[["r_control"]][r_treat_sample_ed]
          rm(r_treat_sample_ed)
          if(is_windows) {
            phi_bootstrap_draw <- parallel::clusterMap(
              cl,
              phi_comp_bootstrap,
              r_treat = r_treat_sample,
              r_control = r_control_sample,
              MoreArgs = list(
                estwind = estwind,
                eventwind = eventwind,
                sigma_cutoff = sigma_cutoff
              ),
              USE.NAMES = FALSE,
              .scheduling = scheduling
            )
          } else {
            phi_bootstrap_draw <- parallel::mcmapply(
              phi_comp_bootstrap,
              r_treat = r_treat_sample,
              r_control = r_control_sample,
              MoreArgs = list(
                estwind = estwind,
                eventwind = eventwind,
                sigma_cutoff = sigma_cutoff
              ),
              SIMPLIFY = FALSE,
              USE.NAMES = FALSE,
              mc.cores = ncores,
              mc.preschedule = static_scheduling
            )
          }
          rm(r_control_sample)
        }
        rm(r_treat_sample)
        n_results_bootstrap_draw <- sum(!vapply(phi_bootstrap_draw, is.null, logical(1L), USE.NAMES = FALSE), na.rm = TRUE)
        if(n_results_bootstrap_draw == 0L) {
          return(list(n_results_bootstrap_draw = n_results_bootstrap_draw, phi_bootstrap_draw = NULL))
        }
        phi_bootstrap_draw <- data.table::rbindlist(phi_bootstrap_draw)
        # compute phi - equ. (7)
        phi_bootstrap_draw <- phi_bootstrap_draw[, .(phi = sum(car_wgted) / sum(one_div_sigma)), by = "tau"]
        return(list(n_results_bootstrap_draw = n_results_bootstrap_draw, phi_bootstrap_draw = phi_bootstrap_draw))
      })
      rm(dp)

      if((!is_single_core) && is_windows) {
        mirai::stop_cluster(cl)
      }

      # get number of bootstrap treatment effects
      n_bootstrap <- sum(vapply(phi_bootstrap, `[[`, integer(1L), "n_results_bootstrap_draw", USE.NAMES = FALSE), na.rm = TRUE)
      if(n_bootstrap == 0L) {
        stop("bootstrapping did not produce any results.")
      }
      phi_bootstrap <- data.table::rbindlist(lapply(phi_bootstrap, `[[`, "phi_bootstrap_draw"))

      # calculate standard errors
      se_phi_bootstrap <- phi_bootstrap[, .(stderr = stats::sd(phi, na.rm = TRUE)), by = "tau"]

      # merge baseline phi to bootstrap standard errors
      se_phi_bootstrap <- se_phi_bootstrap[res[["phi"]], on = "tau"]
      data.table::setcolorder(se_phi_bootstrap, c("tau", "phi", "stderr"))

      # calculate (two-sided) p-value (H0: phi = 0)
      se_phi_bootstrap[, pval := (1 - stats::pnorm(abs(phi / stderr)))*2]

      # calculate CI intervals
      # se_phi_bootstrap[, c("ci_90_lower", "ci_90_upper") := list(phi - stderr * stats::qnorm(0.95), phi + stderr * stats::qnorm(0.95))]
      se_phi_bootstrap[, c("ci_95_lower", "ci_95_upper") := list(phi - stderr * stats::qnorm(0.975), phi + stderr * stats::qnorm(0.975))]
      # se_phi_bootstrap[, c("ci_99_lower", "ci_99_upper") := list(phi - stderr * stats::qnorm(0.995), phi + stderr * stats::qnorm(0.995))]

      # return all information of interest
      out[["ate"]] <- se_phi_bootstrap[]
      out[["ar"]] <- res[["ar"]]
      out[["ate_bootstrap"]] <- phi_bootstrap
      out[["n_bootstrap"]] <- n_bootstrap
    }
  }

  # record the call
  out[["call"]] <- match.call()

  # Define a new class
  class(out) <- "synthReturn"

  return(out)
}
