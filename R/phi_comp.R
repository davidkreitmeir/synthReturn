NULL
###################################################################################
#' Function that computes treatment effect for treatment group.
#'
#' @description \code{phi_comp} Wrapper function to compute \eqn{\phi} for
#' placebo treatment group.
#'
#' @param r_treat_placebo Returns of placebo treatment group.
#' @param r_control Returns of control group (includes placebo treatment group returns.
#'
#' @return A list of 2 data.tables, with the main data.table `phi` containing the following columns:
#'  \item{tau}{Relative time to event day.}
#'  \item{phi}{Average treatment effect.}
#'
#' @import data.table
#' @importFrom data.table .N
#' @importFrom data.table ':='

phi_comp <- function(r_treat, r_control){

  r_treat <- data.table:::split.data.table(r_treat, by = "tid")

  # obtain event panel for each treatment group
  event_panels <- base::lapply(
    r_treat,
    get_event_panel,
    dt_control = r_control
  )

  View(event_panels)

  # compute abnormal returns (ARs) for each placebo treatment group firm
  ARs <- base::lapply(
    event_panels,
    ar_comp
  )

  # compute phi for "actual" treatment group
  ARs <- data.table::setorder(data.table::rbindlist(ARs, use.names=TRUE, idcol="rid")[, rid := base::as.numeric(rid)], rid, tau)
  # compute cumulative abnormal returns (CARs) and sigma
  ARs <- ARs[, car := base::cumsum(ar), by = rid][, car_wgted := car/sigma][, one_div_sigma := 1/sigma]
  # filter out CARs or sigma's that are infinite or missing (perfect prediction by synthetic returns)
  ARs <- ARs[base::is.finite(car_wgted) & !is.na(car_wgted) & base::is.finite(one_div_sigma) & !is.na(one_div_sigma),]
  # compute phi - equ. (7)
  phi <- ARs[, .(num = base::sum(car_wgted), den = base::sum(one_div_sigma)), by = tau]
  phi <- phi[, phi := num/den][,c("tau","phi")]
  ARs <- ARs[, c("rid", "tau", "ar", "sigma")]
  out <- list(phi = phi, ar = ARs)

  return(out)

}

