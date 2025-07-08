

phi_comp_bootstrap <- function(r_treat, r_control, estwind, eventwind, sigma_cutoff) {
  out <- tryCatch({
    # sample control units
    r_control_units <- unique(r_control[, "unit_id"])[["unit_id"]]
    n_control <- length(r_control_units)
    r_control_units <- data.table::data.table(unit_id = sample(r_control_units, n_control, TRUE), new_unit_id = 1:n_control, key = "unit_id")
    r_control <- r_control[r_control_units, -"unit_id", nomatch = NULL, on = "unit_id"]
    data.table::setnames(r_control, "new_unit_id", "unit_id")
    # compute ARs with treatment and control sample for specific event date
    ARs <- event_panel(r_treat, NULL, r_control, estwind, eventwind)
    # If correction is implemented
    if(!is.null(sigma_cutoff)){
      # drop all placebo firms that do not have a good synthetic match
      ARs <- ARs[sigma <= sigma_cutoff,]
    }
    return(ARs)
  }, error = function(x) return(NULL))

  return(out)
}
