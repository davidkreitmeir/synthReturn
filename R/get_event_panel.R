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

get_event_panel <- function(r_treat, r_control){

  # get set of all "potential" control companies for event date
  edate <- r_treat[, .SD[1], by = ed]$ed
  r_event <- r_control[ed == edate,]

  # get dates when treated corporation is traded
  dates <- r_treat[!is.na(r) & is.finite(r), c("d", "eventwind", "estwind")]
  tdates <- dates$d
  # select control companies
  cids <- r_event[!is.na(r) & d %in% tdates, c("cid", "date", "r")]
  # (1) No missing trading days on days treated corporation is traded
  cids <- cids[, tdays:= 1:.N, by = cid]
  cids <- cids[,.(tdays = base::max(tdays)), by = cid]
  # control corp needs exactly the same length of observed trading days as treated corporation
  cids <- cids[tdays >= base::length(tdates)]$cid
  # (2) price changes need to be observed during period
  cids <- r_event[cid %in% cids, c("cid", "r")]
  cids <- cids[r != 0 & !is.na(r),][,.(r_var = .N), by = .(cid)]

  # convert ids and dates to vectors
  cids <- cids$cid

  # filter control corp set
  r_event <- r_event[(d %in% tdates & cid %in% cids), c("cid",  "r", "date")][dates, on = "date"][, d := NULL][ , time := 1:.N, by=cid]
  r_treat <- r_treat[date %in% tdates,][, `:=` (cid = 0, treatid = NULL, stata_eventdate = NULL, d = NULL)][,time := 1:.N,by=cid]

  # combine data.tables
  out <- rbindlist(list(r_event, r_treat), use.names = TRUE)

  return(out)

}

get_event_panel <- purrr::possibly(get_event_panel, otherwise = NULL, quiet = TRUE)
