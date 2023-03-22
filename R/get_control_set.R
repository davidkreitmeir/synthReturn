NULL
###################################################################################
#' Function that pre-processes returns of the control group
#'
#' @description \code{get_control_set} is used to pre-process the returns of the control group
#' and returns an event date specific set of "potential" control companies that fulfill the stated conditions
#'
#'
#' @param eventdate the date on which the event took place.
#' @param ret_control data.table containing that has to contain the following columns: controlid, date, ret
#' @param eventwind An \eqn{n} x \eqn{1} vector of Group indicators (=1 if observation is treated in the post-treatment, =0 otherwise).
#' @param covariates An \eqn{n} x \eqn{k} matrix of covariates to be used in the regression estimation.
#' @param i.weights An \eqn{n} x \eqn{1} vector of weights to be used. If NULL, then every observation has the same weights.
#' @param boot Logical argument to whether bootstrap should be used for inference. Default is FALSE.
#' @param boot.type Type of bootstrap to be performed (not relevant if \code{boot = FALSE}). Options are "weighted" and "multiplier".
#' If \code{boot = TRUE}, default is "weighted".
#' @param nboot Number of bootstrap repetitions (not relevant if \code{boot = FALSE}). Default is 999.
#' @param inffunc Logical argument to whether influence function should be returned. Default is FALSE.
#'
#' @return A data.table containing the following columns:
#'  \item{ATT}{The TWFE DID point estimate}
#'  \item{se}{The TWFE DID standard error}
#'  \item{uci}{Estimate of the upper bound of a 95\% CI for the TWFE parameter.}
#'  \item{lci}{Estimate of the lower bound of a 95\% CI for the TWFE parameter.}
#'  \item{boots}{All Bootstrap draws of the ATT, in case bootstrap was used to conduct inference. Default is NULL}
#'  \item{att.inf.func}{Estimate of the influence function. Default is NULL}
#'
#' @export

get_control_set<- function(
  eventdate,
  ret_control,
  eventwind,
  estwind,
  estobs_min,
  eventobs_min
){

  if(estobs_min)

  # gen relative time variable
  ret_control <- ret_control[, eventdate := eventdate]
  ret_control <- ret_control[, datenum := 1:.N, by = tempid]
  target <- ret_control[, target := ifelse(date == eventdate, datenum, 0)][, .(target = max(target)), by = tempid]
  ret_control <- ret_control[, target := NULL][target, on = .(tempid)]
  ret_control <- ret_control[, tau := datenum - target][, `:=` (target = NULL,datenum = NULL)]
  # create indicator for
  ret_control <- ret_control[, est_wind := ifelse(tau >= estwind[1] & tau < estwind[2], 1,0)][, event_wind := ifelse(tau >= eventwind[1] & tau <= eventwind[2], 1,0) ]
  ret_control <- ret_control[(est_wind == 1 | event_wind == 1),]
  # get no of non-missing trading days for estimation AND event windows
  ret_control <- ret_control[, count_event_obs := sum(!is.na(ret) & is.finite(ret) & event_wind == 1), by = tempid]
  ret_control <- ret_control[, count_est_obs := sum(!is.na(ret) & is.finite(ret) & est_wind == 1), by = tempid]
  # Drop thinly-traded companies or corporations without return variance
  ret_var_ids <- ret_control[ret != 0 & !is.na(ret) & is.finite(ret),][,.(ret_var = .N), by = .(tempid)][ret_var > 0, "tempid"]$tempid
  control_corp_ids <- ret_control[(count_est_obs >= estobs_min) & (count_event_obs >= eventobs_min), .SD[1], by = "tempid"]
  control_corp_ids <- control_corp_ids[tempid %in% ret_var_ids, c("tempid", "eventdate")]
  control_corp_ids <- control_corp_ids$tempid

  # return set of potential control corporations for event date
  ret_control <- ret_control[tempid %in% control_corp_ids, c("tempid", "eventdate", "date", "ret")]

  return(ret_control)

}
