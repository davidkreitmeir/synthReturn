###################################################################################
#' Helper Functions used throughout package
#'

# negation of `%in%`.
`%notin%` <- Negate(`%in%`)

#' @describeIn goodness of match fit function (equation 8) in Acemoglu et al. (2016).
sigmafun <- function(x) {
  return(sqrt(sum(x)^2/ length(x)))
}

# check if data.table, otherwise convert
convert_DT <- function(dta) {
  if(!("data.table" %chin% class(dta))) {
    # warning("class of data object was not data.table; converting...")
    dta <- data.table::as.data.table(dta)
    return(dta)
  } else {
    return(dta)
  }
}

