NULL
###################################################################################
#' Function that pre-processes returns of the control group
#'
#' @description \code{get_event_panel} is used to create final "event panel" data set. Control corporations
#' are chosen such that a balanced panel in relative event time is created.
#'
#' @param ed date of event.
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
#'  \item{cid}{Unique (event \eqn{\times}) control firm identifier.}
#'  \item{d}{Trading Date.}
#'  \item{ed}{Event date}
#'  \item{r}{Stock return.}
#'
#' @import data.table
#' @importFrom purrr possibly

get_event_panel <- function(treat_ret_DT, control_ret_DT){

  # get set of all potential control comps for eventdate
  eventdate <- treat_ret_DT[, .SD[1], by = stata_eventdate]$stata_eventdate
  data <- control_ret_DT[stata_eventdate == eventdate,]

  # get dates when treated corporation is traded
  dates <- treat_ret_DT[!is.na(ret) & is.finite(ret), c("date", "event_wind", "est_wind")]
  tdates <- dates$date
  # select control companies
  cids <- data[!is.na(ret) & date %in% tdates, c("tempid", "date", "ret")]
  # (1) No missing trading days on days treated corporation is traded
  cids <- cids[, tdays:= 1:.N, by = tempid]
  cids <- cids[,.(tdays = max(tdays)), by = tempid]
  # control corp needs exactly the same length of observed trading days as treated corporation
  cids <- cids[tdays >= length(tdates)]$tempid
  # (2) price changes need to be observed during period
  cids <- data[tempid %in% cids, c("tempid", "ret")]
  cids <- cids[ret != 0 & !is.na(ret),][,.(ret_var = .N), by = .(tempid)]

  # convert ids and dates to vectors
  cids <- cids$tempid

  # filter control corp set
  data <- data[(date %in% tdates & tempid %in% cids), c("tempid",  "ret", "date")][dates, on = "date"][, date := NULL][ , time := 1:.N, by=tempid]
  treat_ret_DT <- treat_ret_DT[date %in% tdates,][, `:=` (tempid = 0, treatid = NULL, stata_eventdate = NULL, date = NULL)][,time := 1:.N,by=tempid]

  # combine data.tables
  data <- rbindlist(list(data, treat_ret_DT), use.names = TRUE)

  return(data)

}

get_event_panel <- purrr::possibly(get_event_panel, otherwise = NULL, quiet = TRUE)
