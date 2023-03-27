NULL
###################################################################################
#' Function that
#'
#' @description \code{synthReturn} computes the treatment effect \eqn{\hat{\phi}} using the synthetic
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
#' @param parallel If analysis should be run in parallel with `furrr`. Default is `FALSE` (i.e. analysis is run
#'  sequentially)
#' @param ncore Number of cores used to run analysis (not relevant if parallel = \code{FALSE}).
#' Default is 1 (i.e. run sequentially).
#'
#' @return A data.table containing the following components:
#' \item{tau}{Relative time to event day (\eqn{\tau} = 0).}
#' \item{phi}{The treatment effect estimate for the period \eqn{[0,k]} during the event window.}
#' \item{ci_90_lower}{90\% confidence interval lower bound (not relevant if placebo = \code{FALSE}).}
#' \item{ci_90_upper}{90\% confidence interval upper bound (not relevant if placebo = \code{FALSE}).}
#' \item{ci_95_lower}{95\% confidence interval lower bound (not relevant if placebo = \code{FALSE}).}
#' \item{ci_95_upper}{95\% confidence interval upper bound (not relevant if placebo = \code{FALSE}).}
#' \item{ci_99_lower}{99\% confidence interval lower bound (not relevant if placebo = \code{FALSE}).}
#' \item{ci_99_upper}{99\% confidence interval upper bound (not relevant if placebo = \code{FALSE}).}
#' \item{n_placebo}{Number of placebo treatment effects.}
#'
#' @importFrom future plan
#' @importFrom furrr future_map
#' @import data.table
#' @importFrom infer rep_slice_sample
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
  ndraws = 25,
  ngroup = 2,
  parallel = FALSE,
  ncore = 1
  ){

  if(parallel == TRUE){
    if(ncore == 1){
      warning("Parallelization is not used. Set ncore > 1 to use more than 1 worker.")
    }
    future::plan(multisession, workers = ncore)
  }

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
    ndraws = ndraws,
    ngroup = ngroup,
    parallel = parallel,
    ncore = ncore
  )

  #-----------------------------------------------------------------------------
  # Implement the methods

  r_treat = dp[[1]]
  r_control = dp[[2]]

  # Compute treatment effect `phi` (for "actual" treatment group)
  phi <- phi_comp(
    r_treat = r_treat,
    r_control = r_control
  )

  #-----------------------------------------------------------------------------
  # Create confidence intervals from placebo treatment group treatment effects

  if(placebo == TRUE){

    # unique event dates
    eds <- base::unique(r_treat[, .SD[1], by = tid]$ed)
    # number of treated corporations
    n <- base::length(base::unique(r_treat[, .SD[1], by = tid]$tid))

    ed_placebo <- r_control[, .SD[1], by = c("cid", "ed")][, `:=` (ret = NULL, date = NULL)]
    ed_placebo <- ed_placebo[, ntotal := 1:.N, by = ed]
    # restrict set of placebo event dates by minimum number of control firms in event(-date) panel
    ngroup_min <- base::floor(ngroup*n)
    ed_placebo <- ed_placebo[, .(ntotal = base::max(ntotal)), by = ed][ntotal >= ngroup_min, "ed"]$ed
    # final set of placebo event dates (and potentially placebo treated firms)
    ed_placebo <- ed_placebo[ed %in% ed_placebo,  c("cid", "ed")]

    # `ndraws` random draws of placebo treatment groups of size `n` (with replacement)
    # for each (unique) event date.
    pids <- data.table::split(ed_placebo, by = "ed")
    pids <- furrr::future_map(
      pids,
      infer::rep_slice_sample,
      n = n,
      replace = TRUE,
      reps =  ndraws
      )
    pids <- data.table::rbindlist(pids, use.names = TRUE, idcol="edid")
    pids <- convert_DT(pids)
    pids <- pids[, pid := .GRP, by = .(edid, replicate)]
    pids <- pids[, `:=` (edid = NULL, replicate = NULL)]
    pids <- split(pids, by = "pid")

    # compute treatment effect for all placebo treatment groups
    phi_placebo <- furrr::future_map(
      pids,
      phi_comp_placebo,
      r_control = r_control,
      estwind = estwind
    )

    phi_placebo <- data.table::rbindlist(phi_placebo, use.names = TRUE, idcol="pid")
    # get number of placebo treatment effects
    n_placebo <- base::length(base::unique(phi_placebo[, .SD[1], by = pid]$pid))

    # calculate CI intervals
    phi_CI90 = results[, .(ci_90_lower = quantile(phi_placebo, probs = 0.05), ci_90_upper = quantile(phi_hat, probs = 0.95)), by = "tau"]
    phi_CI95 = results[, .(ci_95_lower = quantile(phi_placebo, probs = 0.025), ci_95_upper = quantile(phi_hat, probs = 0.975)), by = "tau"]
    phi_CI99 = results[, .(ci_99_lower = quantile(phi_hat, probs = 0.005), ci_99_upper = quantile(phi_hat, probs = 0.995)), by = "tau"]

    out = phi[phi_CI90, on = .(tau)][phi_CI95, on = .(tau)][phi_CI99, on = .(tau)][, n_placebo := n_placebo]

    # return "actual" treatment effects with CIs
    return(out)

  } else {
    # just return treatment effects without CIs
    return(phi)
  }

}

