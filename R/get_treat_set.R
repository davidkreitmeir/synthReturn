###################################################################################
#' Function that pre-processes returns of the control group
#'
#' @description \code{get_treat_set} is used to create "event panels" for firms in the treatment group.
#'
#' @param eventdate date of event.
#' @param data The name of the data.table that contains the data.
#' @param estwind Argument to set estimation window period in relative time to event, i.e. `c(estwind_start, estwind_end)`
#' @param eventwind Argument to set event window period in relative time to event, i.e. `c(eventwind_start, eventwind_end)`
#' @param estobs_min Argument to define minimum number of trading days during the estimation window.
#' Can be an integer or a proportional (i.e. between 0 and 1). Default is \eqn{1}, i.e. no missing trading days
#' are allowed.
#' @param estobs_min Argument to define minimum number of trading days during the event window. Can be an
#' integer or a proportional (i.e. between 0 and 1). Default is \eqn{1}, i.e. no missing trading days are allowed.
#'
#' @return A data.table containing the following columns:
#'  \item{tid}{Unique (event \eqn{\times}) treatment firm identifier.}
#'  \item{d}{Trading Date.}
#'  \item{ed}{Event date}
#'  \item{r}{Stock return.}
#'  \item{estwind}{Indicator variable for estimation window. Equal to 1 if trading day during estimation window,
#'  and 0 otherwise.}
#'  \item{eventobs_min}{Indicator variable for event window. Equal to 1 if trading day during event window,
#'  and 0 otherwise.}
#'

get_treat_set <- function(
  eventdate,
  out,
  estwind,
  eventwind,
  estobs_min,
  eventobs_min
) {

  # make sure code does not break because of an error during calculation of a specific corporation
  out <- tryCatch({
    # get relative time variable
    out[, tau := (1:.N) - which(d == eventdate)]
    # drop units not observed on event date
    out <- na.omit(out, cols = "tau")
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
  }, error = function(x) return(NULL))

  return(out)
}
