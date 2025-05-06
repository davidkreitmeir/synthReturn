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

phi_comp <- function(r_treat, r_control, ncores, is_windows) {

  # save mapping of unit ID to row ID (`rid`)
  unit_id_rid_map <- unique(r_treat[, "unit_id"])
  unit_id_rid_map[, rid := 1:.N]
  r_treat <- split(r_treat, by = "unit_id")

  if(ncores == 1L) {
    # obtain event panel for each treatment group
    # compute abnormal returns (ARs) for each placebo treatment group firm
    ARs <- data.table::rbindlist(
      lapply(
        r_treat,
        event_panel,
        dt_control = r_control
      ),
      use.names = TRUE,
      idcol = "rid"
    )
  } else {
    if(is_windows) {
      ARs <- data.table::rbindlist(
        mirai::mirai_map(
          r_treat,
          event_panel,
          .args = list(
            dt_control = r_control
          ),
          .compute = "synthReturn"
        )[],
        use.names = TRUE,
        idcol = "rid"
      )
    } else {
      ARs <- data.table::rbindlist(
        parallel::mclapply(
          r_treat,
          event_panel,
          dt_control = r_control,
          mc.cores = ncores
        ),
        use.names = TRUE,
        idcol = "rid"
      )
    }
  }

  # compute phi for "actual" treatment group
  ARs[, rid := as.numeric(rid)]
  data.table::setorder(ARs, rid, tau)
  # compute cumulative abnormal returns (CARs) and sigma
  ARs[, car := cumsum(ar), by = "rid"]
  ARs[, c("car_wgted", "one_div_sigma") := list(car / sigma, 1 / sigma)]
  # filter out CARs or sigma's that are infinite or missing (perfect prediction by synthetic returns)
  ARs <- ARs[is.finite(car_wgted) & is.finite(one_div_sigma),]
  # compute phi - equ. (7)
  phi <- ARs[, .(phi = sum(car_wgted) / sum(one_div_sigma)), by = "tau"]
  # map treatment IDs back to ARs
  ARs <- ARs[unit_id_rid_map, c("unit_id", "tau", "ar", "sigma"), on = "rid"]
  out <- list(phi = phi, ar = ARs)

  return(out)
}

