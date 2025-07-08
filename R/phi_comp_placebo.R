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

phi_comp_placebo <- function(placebo_treat_ids, r_control_ed, estwind, eventwind, sigma_cutoff) {

  # p_treat: vector of placebo treatment group unit ids

  out <- tryCatch({

    # assign new unit ids because placebo group was drawn with replacement, and multiple instances of the same unit must be considered separate
    placebo_treat_ids <- data.table::data.table(unit_id = placebo_treat_ids, new_unit_id = 1:length(placebo_treat_ids))

    # get returns of placebo treatment firms
    r_treat_placebo <- r_control_ed[placebo_treat_ids, -"unit_id", nomatch = NULL, on = "unit_id"]

    # split by placebo treated firm
    r_treat_placebo <- split(r_treat_placebo, by = "new_unit_id", keep.by = FALSE)

    placebo_treat_ids[, new_unit_id := NULL]

    # drop placebo treated companies from control group
    r_control_placebo <- r_control_ed[!placebo_treat_ids, on = "unit_id"]

    # obtain event panel for each treatment group
    # compute abnormal returns (ARs) for each placebo treatment group firm
    ARs <- data.table::rbindlist(
      lapply(
        r_treat_placebo,
        event_panel,
        treat_ed = NULL,
        dt_control = r_control_placebo,
        estwind = estwind,
        eventwind = eventwind
      )
    )

    # If correction is implemented
    if(!is.null(sigma_cutoff)){
      # drop all placebo firms that do not have a good synthetic match
      ARs <- ARs[sigma <= sigma_cutoff,]
    }

    # compute phi - equ. (7)
    phi <- ARs[, .(phi = sum(car_wgted) / sum(one_div_sigma)), by = "tau"]
    return(phi)
  }, error = function(x) return(NULL))

  return(out)
}
