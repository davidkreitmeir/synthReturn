#' @title Simulated timeseries of stock returns
#'
#' @description `ret_two_evdates` presents a simulated timeseries of stock returns for 20
#' treated firms and a set of 60 potential control firms. The simulated data allows for a max
#' estimation window of 100 trading days (in event time: -100 to -1) and a max event window
#' of 6 trading days (in event time: 0 to +5).
#'
#' @format `ret_two_evdates` has 14,000 rows and 5 variables:
#' \describe{
#'   \item{eventdate}{Calendar date when the event happened (`<Date>`).}
#'   \item{date}{Calendar date (`<Date>`).}
#'   \item{ret}{Raw stock return (`<num>`).}
#'   \item{treat}{Binary treatment indicator (`<lgcl>`).}
#'   \item{unit}{Unique identifier for each treated, respectively control firm (`<int>`).}
#' }
"ret_two_evdates"
