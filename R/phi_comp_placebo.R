NULL
###################################################################################
#' Function that computes treatment effect for placebo treatment group.
#'
#' @description \code{phi_comp_placebo} Wrapper function to compute \eqn{\phi} for
#' placebo treatment group.
#'
#' @param r_treat_placebo Returns of placebo treatment group.
#' @param r_control Returns of control group (includes placebo treatment group returns.
#' @param estwind Argument to set estimation window period in relative time to event,
#' i.e. `c(estwind_start, estwind_end)`
#'
#' @return A data.table containing the following columns:
#'  \item{tau}{Relative time to event date.}
#'  \item{phi}{(Placebo) treatment effect.}
#'
#' @import data.table
#' @importFrom data.table .N
#' @importFrom data.table ':='
#' @importFrom purrr possibly

phi_comp_placebo <- function(pids, r_control, estwind){

  # get key information on placebo treatment group
  edate <- unique(pids[,"ed"])$ed
  tids <- unique(pids[,"cid"])$cid

  # get returns of treated corporations
  r_treat_placebo <- r_control[(cid %in% tids &  ed == edate),]
  r_treat_placebo <- r_treat_placebo[, tid := .GRP, by = .(cid)][, cid := NULL]
  # add event and estimation window
  estwindlen = abs(estwind[1] - estwind[2])
  r_treat_placebo <- r_treat_placebo[, time := 1:.N, by = "tid"][, `:=` (estwind = ifelse(time <= estwindlen, 1, 0), eventwind = ifelse(time > estwindlen, 1, 0), time = NULL)]
  # split by placebo treated firm
  r_treat_placebo <- data.table::split(r_treat_placebo, by = "tid")

  # drop placebo treated companies from control group
  r_control <- r_control[(cid %notin% tids &  ed == edate),]

  # obtain event panel for each treatment group
  event_panels <- base::lapply(
    r_treat_placebo,
    get_event_panel,
    r_control = r_control
  )


  # compute abnormal returns (ARs) for each placebo treatment group firm
  ARs <- base::lapply(
    event_panels,
    ar_comp
  )

  # compute phi for placebo treatment group
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

phi_comp_placebo <- purrr::possibly(phi_comp_placebo, otherwise = NULL, quiet = TRUE)
