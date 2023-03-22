#' @describeIn negation of `%in%`.
#' @export
`%notin%` <- Negate(`%in%`)

#' @describeIn goodness of match fit function (equation 8) in Acemoglu et al. (2016).
#' @export
sigmafun <- function (x){
  return ( sqrt ( sum(x)^2/ length (x)))
}
