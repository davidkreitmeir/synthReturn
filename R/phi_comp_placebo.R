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

phi_comp_placebo <- function(p_treat_unit_id, treat_ed, dt_control, estwind, eventwind) {

  # p_treat: placebo treatment group

  out <- tryCatch({

    # get control group for that event date
    dt_control_edate <- dt_control[[treat_ed]]
    if(is.null(dt_control_edate)) {
      return(NULL)
    }

    # get returns of placebo treatment firms
    r_treat_placebo <- dt_control_edate[p_treat_unit_id, nomatch = NULL, on = "unit_id"]

    # split by placebo treated firm
    r_treat_placebo <- split(r_treat_placebo, by = "unit_id")

    # drop placebo treated companies from control group
    r_control_placebo <- dt_control_edate[!p_treat_unit_id, on = "unit_id"]

    # obtain event panel for each treatment group
    # compute abnormal returns (ARs) for each placebo treatment group firm
    ARs <- data.table::rbindlist(
      mapply(
        event_panel,
        dt_treat = r_treat_placebo,
        treat_ed = treat_ed,
        MoreArgs = list(
          dt_control = r_control_placebo,
          estwind = estwind,
          eventwind = eventwind
        ),
        SIMPLIFY = FALSE,
        USE.NAMES = FALSE
      ),
      idcol = "panel_id"
    )

    # compute phi - equ. (7)
    phi <- ARs[, .(phi = sum(car_wgted) / sum(one_div_sigma)), by = "tau"]
    return(phi)
  }, error = function(x) return(NULL))

  return(out)
}
