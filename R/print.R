##########
## Print
##########
## a synthReturn object


print.synthReturn <-  function(x,
                               ...) {

  cat("Call:\n")
  print(x[["arg"]], digits = 4)

  if (x[["arg"]][["inference"]] == 'none') {
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
