% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_treat_set.R
\name{get_treat_set}
\alias{get_treat_set}
\title{Function that pre-processes returns of the control group}
\usage{
get_treat_set(...)
}
\arguments{
\item{eventdate}{date of event.}

\item{data}{The name of the data.table that contains the data.}

\item{estwind}{Argument to set estimation window period in relative time to event, i.e. \code{c(estwind_start, estwind_end)}}

\item{eventwind}{Argument to set event window period in relative time to event, i.e. \code{c(eventwind_start, eventwind_end)}}

\item{estobs_min}{Argument to define minimum number of trading days during the event window. Can be an
integer or a proportional (i.e. between 0 and 1). Default is \eqn{1}, i.e. no missing trading days are allowed.}
}
\value{
A data.table containing the following columns:
\item{tid}{Unique (event \eqn{\times}) treatment firm identifier.}
\item{d}{Trading Date.}
\item{ed}{Event date}
\item{r}{Stock return.}
\item{estwind}{Indicator variable for estimation window. Equal to 1 if trading day during estimation window,
and 0 otherwise.}
\item{eventobs_min}{Indicator variable for event window. Equal to 1 if trading day during event window,
and 0 otherwise.}
}
\description{
\code{get_treat_set} is used to create "event panels" for firms in the treatment group.
}
