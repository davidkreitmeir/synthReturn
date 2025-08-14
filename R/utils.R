###################################################################################
## Helper Functions used throughout package
###################################################################################


# Goodness of match fit function (equation 8) in Acemoglu et al. (2016).
sigmafun <- function(x) {
  return(sqrt(sum(x, na.rm = TRUE) ^ 2L / length(x)))
}

# Avoid R CMD check note
utils::globalVariables(
  c(".", "ar", "car", "car_wgted", "d", "ed", "estwind", "eventwind", "one_div_sigma",
    "phi", "r", "r_var", "sigma", "tau", "tdays", "unit_id", "na.omit", "n_est_obs",
    "n_event_obs", "..unitname", "..dname", "..edname", "..rname", "pval_lt",
    "pval_ut", "phi_permut", "pval"))
