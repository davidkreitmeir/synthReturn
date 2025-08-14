###################################################################################
## Function to pre-processe the returns of the control group for a given
## event date
###################################################################################

get_control_set <- function(
  eventdate,
  cdata,
  estwind,
  eventwind,
  estobs_min,
  eventobs_min,
  ncontrol_min,
  not_permutation
) {

  out <- data.table::copy(cdata)
  # get relative time variable
  out[, tau := (1:.N) - which(d == eventdate), by = "unit_id"]
  # drop units not observed on event date
  out <- stats::na.omit(out, cols = "tau")
  # subset to observations in event and estimation windows
  out <- out[(tau %between% estwind) | (tau %between% eventwind),]
  # subset to units with enough observations in estimation window
  keep_units <- out[tau %between% estwind, .(n_est_obs = .N), by = "unit_id"][n_est_obs >= estobs_min, "unit_id"]
  out <- out[keep_units, nomatch = NULL, on = "unit_id"]
  # subset to units with enough observations in event window
  keep_units <- out[tau %between% eventwind, .(n_event_obs = .N), by = "unit_id"][n_event_obs >= eventobs_min, "unit_id"]
  if(not_permutation) {
    out[, tau := NULL]
  }
  out <- out[keep_units, nomatch = NULL, on = "unit_id"]
  # subset to units with return variance during entire sample period
  keep_units <- out[, .(r_var = stats::var(r, na.rm = TRUE)), by = "unit_id"][r_var > 0, "unit_id"]
  if(nrow(keep_units) < ncontrol_min) {
    return(NULL)
  }
  out <- out[keep_units, nomatch = NULL, on = "unit_id"]

  return(out)
}
