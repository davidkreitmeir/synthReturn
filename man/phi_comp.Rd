% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/phi_comp.R
\name{phi_comp}
\alias{phi_comp}
\title{Function that computes treatment effect for treatment group.}
\usage{
phi_comp(r_treat, r_control)
}
\arguments{
\item{r_treat}{Returns of placebo treatment group.}

\item{r_control}{Returns of control group (includes placebo treatment group returns.}
}
\value{
A list containing the following components:
\item{phi}{Data.frame containing the average treatment effect estimates \eqn{\phi} by relative event day \eqn{\tau}.}
\item{ar}{Data.frame containing the estimated abnormal returns, and the "goodness" of the synthetic match estimate \eqn{\sigma} for all firms in the (actual) treatment group.}
}
\description{
\code{phi_comp} Wrapper function to compute \eqn{\phi} for
placebo treatment group.
}
