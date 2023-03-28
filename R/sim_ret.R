#' @title Simulated repeated cross-section data
#'
#' @description \code{sim_rc} contains 3 simulated stock return data sets
#'
#'
#' @format The main data frame `ret_two_evdates_na` has 14000 rows and 7 variables:
#' \describe{
#'   \item{treatid}{unique "firm x event" identifier for each firm in the treatment group.
#'   NA if firm not in treatment group.}
#'   \item{date}{date variable}
#'   \item{ret}{raw stock return}
#'   \item{estwind}{an indicator variable for the estimation window. Equal to 1 if
#'   the trading day is in the estimation window; equal to 0 if the trading day is outside
#'   the estimation window.}
#'   \item{eventwind}{an indicator variable for the event window. Equal to 1 if
#'   the trading day is in the event window; equal to 0 if the trading day is outside
#'   the event window.}
#'   \item{eventdate}{date when the event happened}
#'   \item{treatid}{unique identifier for each firm in the control group.
#'   NA if firm in treatment group.}
#
#' }
"ret_one_evdate"
"ret_two_evdates_na"
"ret_two_evdates"
