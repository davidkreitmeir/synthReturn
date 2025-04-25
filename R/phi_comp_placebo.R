NULL
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

phi_comp_placebo <- function(placebo_treatment_group, dt_control, estwind, ncores, is_windows) {

  out <- tryCatch({
    # get key information on placebo treatment group
    edate <- unique(placebo_treatment_group[, "ed"])[["ed"]]
    tids <- unique(placebo_treatment_group[, "cid"])

    # get returns of placebo treatment firms
    r_treat_placebo <- dt_control[ed == edate,][tids, nomatch = NULL, on = "cid"]
    r_treat_placebo[, tid := .GRP, by = "cid"]
    r_treat_placebo[, cid := NULL]

    # add event and estimation window
    estwindlen <- abs(estwind[1L] - estwind[2L]) + 1L
    r_treat_placebo[, time := 1:.N, by = "tid"]
    r_treat_placebo[, c("estwind", "eventwind") := list(as.integer(time <= estwindlen), as.integer(time > estwindlen))]
    r_treat_placebo[, time := NULL]

    # split by placebo treated firm
    r_treat_placebo <- split(r_treat_placebo, by = "tid")

    # drop placebo treated companies from control group
    r_control_placebo <- dt_control[ed == edate,][!tids, on = "cid"]

    if(ncores == 1L) {
      # obtain event panel for each treatment group
      event_panels <- lapply(
        r_treat_placebo,
        get_event_panel,
        dt_control = r_control_placebo
      )
      # compute abnormal returns (ARs) for each placebo treatment group firm
      ARs <- data.table::rbindlist(
        lapply(
          event_panels,
          ar_comp
        ),
        use.names = TRUE,
        idcol = "rid"
      )
    } else {
      if(is_windows) {
        mirai::daemons(ncores)
        event_panels <- mirai::mirai_map(
          r_treat_placebo,
          get_event_panel,
          .args = list(
            dt_control = r_control_placebo
          )
        )[]
        ARs <- data.table::rbindlist(
          mirai::mirai_map(
            event_panels,
            ar_comp
          )[],
          use.names = TRUE,
          idcol = "rid"
        )
        mirai::daemons(0L)
      } else {
        event_panels <- parallel::mclapply(
          r_treat_placebo,
          get_event_panel,
          dt_control = r_control_placebo,
          mc.cores = ncores
        )
        ARs <- data.table::rbindlist(
          parallel::mclapply(
            event_panels,
            ar_comp,
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
    ARs[, c("car_wgted", "one_div_sigma") := list(car/sigma, 1/sigma)]
    # filter out CARs or sigma's that are infinite or missing (perfect prediction by synthetic returns)
    ARs <- ARs[is.finite(car_wgted) & is.finite(one_div_sigma),]
    # compute phi - equ. (7)
    phi <- ARs[, .(num = sum(car_wgted), den = sum(one_div_sigma)), by = "tau"]
    phi[, phi := num/den]
    phi <- phi[, c("tau", "phi")]
    # ARs <- ARs[, c("rid", "tau", "ar", "sigma")]
    # out <- list(phi = phi, ar = ARs)
    return(phi)
  }, error = function(x) return(NULL))

  return(out)
}
