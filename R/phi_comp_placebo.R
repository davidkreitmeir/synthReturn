###################################################################################
#' Function that computes treatment effect for placebo treatment group.
#'
#' @description \code{phi_comp_placebo} Wrapper function to compute \eqn{\phi} for
#' placebo treatment group.
#'
#' @param r_treat_placebo Returns of placebo treatment group.
#' @param dt_control Returns of control group (includes placebo treatment group returns.
#' @param estwind Argument to set estimation window period in relative time to event,
#' i.e. `c(estwind_start, estwind_end)`
#'
#' @return A data.table containing the following columns:
#'  \item{tau}{Relative time to event date.}
#'  \item{phi}{(Placebo) treatment effect.}
#'

phi_comp_placebo <- function(placebo_treat_id, r_control_ed, estwind, eventwind, sigma_cutoff) {

  # placebo_treat_id: placebo treatment unit id

  # get returns of placebo treatment firms
  r_treat_placebo <- r_control_ed[.(placebo_treat_id), c("d", "r", "tau"), nomatch = NULL, on = "unit_id"]

  # drop placebo treated unit from control group
  r_control_placebo <- r_control_ed[!.(placebo_treat_id), on = "unit_id"]

  # return directly if no control unit left
  if(nrow(r_control_placebo) == 0L) {
    return(NULL)
  }

  # obtain event panel for each treatment group
  # compute abnormal returns (ARs) for each placebo treatment group firm
  ARs <- event_panel(r_treat_placebo, r_control_placebo, estwind, eventwind)

  if(is.null(ARs)) {
    return(NULL)
  }

  # If correction is implemented
  if(!is.null(sigma_cutoff)){
    # drop all placebo firms that do not have a good synthetic match
    ARs <- ARs[sigma <= sigma_cutoff,]
  }

  # compute phi - equ. (7)
  phi <- ARs[, .(phi = sum(car_wgted) / sum(one_div_sigma)), by = "tau"]
  return(phi)
}
