##########
## Print
##########
## a synthReturn object

#' @exportS3Method synthReturn::print
print.synthReturn <-  function(x,
                               ...) {

  if (x[["call"]][["inference"]] == 'none') {
    cat("\nAverage Treatment Effect on the Treated:\n")
    print(
      round(as.data.frame(x[["ate"]]), digits = 4),
      row.names = FALSE,
      scientific = FALSE,
    )
    cat("\nUncertainty estimates not available.\n")
  } else {
    cat("\nAverage Cumulative Treatment Effect:\n")
    print(
      round(as.data.frame(x[["ate"]]), digits = 4),
      row.names = FALSE,
      scientific = FALSE,
    )
  }
}
