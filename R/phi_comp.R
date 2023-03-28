NULL
###################################################################################
#' Function that computes treatment effect for treatment group.
#'
#' @description \code{phi_comp} Wrapper function to compute \eqn{\phi} for
#' placebo treatment group.
#'
#' @param r_treat_placebo Returns of placebo treatment group.
#' @param r_control Returns of control group (includes placebo treatment group returns.
#' @param estwind Argument to set estimation window period in relative time to event,
#' i.e. `c(estwind_start, estwind_end)`
#'
#' @return A data.table containing the following columns:
#'  \item{tau}{Relative time to event date.}
#'  \item{phi}{Abnormal return.}
#'
#' @import data.table
#' @importFrom furrr future_map


phi_comp <- function(r_treat, r_control, estwind){

  r_treat <- data.table::split(r_treat, by = "tid")

  # obtain event panel for each treatment group
  event_panels <- furrr::future_map(
    r_treat,
    get_event_panel,
    r_control = r_control
  )


  # compute abnormal returns (ARs) for each placebo treatment group firm
  ARs <- furrr::future_map(
    event_panels,
    ar_comp
  )

  # compute phi for treatment group
  phi <- data.table::setorder(data.table::rbindlist(ARs, use.names=TRUE, idcol="rid"), rid, tau)
  # compute cumulative abnormal returns (CARs) and sigma
  phi <- phi[, CAR := base::cumsum(AR), by = rid][, CAR_wgted := CAR/sigma][, one_div_sigma := 1/sigma]
  # filter out CARs or sigma's that are infinite or missing (perfect prediction by synthetic returns)
  phi <- phi[base::is.finite(CAR_wgted) & !is.na(CAR_wgted) & base::is.finite(one_div_sigma) & !is.na(one_div_sigma),]
  # compute phi - equ. (7)
  phi <- phi[, .(num = base::sum(CAR_wgted), den = base::sum(one_div_sigma)), by = tau]
  out <- phi[, phi := num/den][,c("tau","phi")]


  return(out)

}

