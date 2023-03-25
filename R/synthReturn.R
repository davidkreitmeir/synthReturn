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
#' @param ndraws Number of randomly drawn placebo treatment group at each (unique) event date.
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
#'
#' @importFrom future plan
#' @importFrom furrr future_map
#' @import data.table
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
    eventobs_min = eventobs_min
  )

  #-----------------------------------------------------------------------------
  # Implement the methods
  # First panel data





}
