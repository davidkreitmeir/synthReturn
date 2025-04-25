###################################################################################
#' Helper Functions used throughout package
#'

#' @describeIn goodness of match fit function (equation 8) in Acemoglu et al. (2016).
sigmafun <- function(x) {
  return(sqrt(sum(x)^2/ length(x)))
}

utils::globalVariables(c(".", "ar", "car", "car_wgted", "cid", "d", "datenum", "den", "ed", "est_wind", "estwind", "event_wind", "eventwind", "n_est_obs",
  "n_event_obs", "ntotal", "num", "one_div_sigma", "phi", "pid", "r", "r_var", "rid", "sigma", "sigma", "synth", "targetv", "tau", "tdays", "tid", "time",
  "treated"))
