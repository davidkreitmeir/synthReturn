###################################################################################
# Function that pre-processes returns of the treatment group
###################################################################################


get_treat_set <- function(
  eventdate,
  out,
  estwind,
  eventwind,
  estobs_min,
  eventobs_min
) {

  # get relative time variable
  out[, tau := (1:.N) - which(d == eventdate)]
  # drop units not observed on event date
  out <- stats::na.omit(out, cols = "tau")
  # subset to observations in event and estimation windows
  out <- out[(tau %between% estwind) | (tau %between% eventwind),]
  # subset to units with enough observations in estimation window
  if(sum(out[["tau"]] %between% estwind, na.rm = TRUE) < estobs_min) {
    return(NULL)
  }
  # subset to units with enough observations in event window
  if(sum(out[["tau"]] %between% eventwind, na.rm = TRUE) < eventobs_min) {
    return(NULL)
  }
  # subset to units with return variance during entire sample period
  if(stats::var(out[["r"]], na.rm = TRUE) == 0) {
    return(NULL)
  }

  # return "event panels" of units in treatment group for event d
  return(out)
}
