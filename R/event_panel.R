###################################################################################
## Function to that calculates the abnormal returns (ARs) for the treatment
## group and control group for a given event date
###################################################################################

event_panel <- function(dt_treat, r_control, estwind, eventwind) {

  # dt_treat: data table; columns: d, r; sorted by d; single unit_id
  # dt_control: list of ed-specific data tables; columns: unit_id, d, r; sorted by unit_id, d; list elements are named according to ed value

  ndt_treat <- nrow(dt_treat)

  # select control companies
  # (1) No missing trading days on days treated corporation is traded
  cids <- r_control[dt_treat[, "d"], "unit_id", nomatch = NULL, on = "d"][, .(tdays = .N), by = "unit_id"][tdays == ndt_treat, "unit_id"]
  # (2) price changes need to be observed during sample period
  cids <- r_control[cids, c("unit_id", "r"), nomatch = NULL, on = "unit_id"][, .(r_var = stats::var(r, na.rm = TRUE)), by = "unit_id"][r_var > 0, "unit_id"]

  if(nrow(cids) == 0L) {
    return(NULL)
  }

  # filter control corp set
  r_control <- r_control[cids, nomatch = NULL, on = "unit_id"][dt_treat[, "d"], c("unit_id", "r"), nomatch = NULL, on = "d"]
  rm(cids)
  data.table::setorder(r_control, unit_id)
  r_control[, tau := rep_len(dt_treat[["tau"]], nrow(r_control))]

  ARs <- ar_comp(dt_treat, r_control, estwind, eventwind)
  if(is.null(ARs)) {
    return(NULL)
  }

  # compute cumulative abnormal returns (CARs) and sigma
  ARs[, c("car_wgted", "one_div_sigma") := list(cumsum(ar) / sigma, 1 / sigma)]
  # filter out CARs or sigma's that are infinite or missing (perfect prediction by synthetic returns)
  ARs <- ARs[is.finite(car_wgted) & is.finite(one_div_sigma),]

  # ARs: data table with rows in eventwind period; columns: ar, sigma, tau, car_wgted, one_div_sigma

  return(ARs)
}
