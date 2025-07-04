

phi_comp_bootstrap <- function(r_treat, r_control, estwind, eventwind) {
  out <- tryCatch({
    # sample control units
    r_control_units <- unique(r_control[, "unit_id"])
    n_control <- nrow(r_control_units)
    r_control_units <- r_control_units[sample.int(n_control, n_control, TRUE),]
    r_control <- r_control[r_control_units, nomatch = NULL, on = "unit_id"]
    # compute ARs with treatment and control sample for specific event date
    ARs <- event_panel(r_treat, NULL, r_control, estwind, eventwind)
    return(ARs)
  }, error = function(x) return(NULL))

  return(out)
}
