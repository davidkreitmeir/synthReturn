###################################################################################
#' Helper Functions used throughout package
#' @import data.table
#'
#'
#' @describeIn negation of `%in%`.
#' @export
`%notin%` <- Negate(`%in%`)

#' @describeIn goodness of match fit function (equation 8) in Acemoglu et al. (2016).
#' @export
sigmafun <- function (x){
  return ( base::sqrt ( base::sum(x)^2/ base::length (x)))
}

#' @describeIn check if data.table, otherwise convert
#' @export
convert_DT <- function(dta){
  if (!all( base::class(dta) == "data.table")) {
    warning("class of data object was not data.table; converting...")
    dta <- data.table::as.data.table(dta)
    return(dta)
  } else {
    return(dta)
  }
}

