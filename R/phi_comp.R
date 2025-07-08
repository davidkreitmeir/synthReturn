###################################################################################
#' Function that computes treatment effect for treatment group.
#'
#' @description \code{phi_comp} Wrapper function to compute \eqn{\phi} for
#' placebo treatment group.
#'
#' @param r_treat Returns of placebo treatment group.
#' @param r_control Returns of control group (includes placebo treatment group returns.
#'
#' @return A list containing the following components:
#' \item{phi}{Data.frame containing the average treatment effect estimates \eqn{\phi} by relative event day \eqn{\tau}.}
#' \item{ar}{Data.frame containing the estimated abnormal returns, and the "goodness" of the synthetic match estimate \eqn{\sigma} for all firms in the (actual) treatment group.}
#'

phi_comp <- function(r_treat, r_control, r_treat_ed, estwind, eventwind, ncores, static_scheduling, is_windows) {

  # r_treat: list of unit-specific data tables; columns: d, r; sorted by d
  # r_control: list of ed-specific data tables; columns: unit_id, d, r; sorted by unit_id, d; list elements are named according to ed value
  # r_treat_unit_ed: data table of event date per treated unit; columns: ed, unit_id; sorted by unit_id

  if(ncores == 1L) {
    # obtain event panel for each treatment group
    # compute abnormal returns (ARs) for each placebo treatment group firm
    ARs <- data.table::rbindlist(
      mapply(
        event_panel,
        dt_treat = r_treat,
        treat_ed = r_treat_ed,
        MoreArgs = list(
          dt_control = r_control,
          estwind = estwind,
          eventwind = eventwind
        ),
        SIMPLIFY = FALSE,
        USE.NAMES = FALSE
      )
    )
  } else {
    if(is_windows) {
      cl <- mirai::make_cluster(ncores)
      ARs <- data.table::rbindlist(
        parallel::clusterMap(
          cl,
          event_panel,
          dt_treat = r_treat,
          treat_ed = r_treat_ed,
          MoreArgs = list(
            dt_control = r_control,
            estwind = estwind,
            eventwind = eventwind
          ),
          USE.NAMES = FALSE,
          .scheduling = data.table::fifelse(static_scheduling, "static", "dynamic")
        )
      )
      mirai::stop_cluster(cl)
    } else {
      ARs <- data.table::rbindlist(
        parallel::mcmapply(
          event_panel,
          dt_treat = r_treat,
          treat_ed = r_treat_ed,
          MoreArgs = list(
            dt_control = r_control,
            estwind = estwind,
            eventwind = eventwind
          ),
          SIMPLIFY = FALSE,
          USE.NAMES = FALSE,
          mc.cores = ncores,
          mc.preschedule = static_scheduling
        )
      )
    }
  }

  if(nrow(ARs) == 0L) {
    stop("phi could not be computed. Make sure that (i) there are control units observed on all observed days of treatment units and (ii) their returns ",
      "vary over time.")
  }
  # compute phi - equ. (7)
  phi <- ARs[, .(phi = sum(car_wgted) / sum(one_div_sigma)), by = "tau"]
  out <- list(phi = phi, ar = ARs)

  return(out)
}

