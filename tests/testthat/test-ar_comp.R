test_that("ar_comp exists as internal function", {
  # Since ar_comp is an internal function and hard to test directly,
  # we test it indirectly through synthReturn
  data("ret_two_evdates")

  # This test ensures ar_comp works by running synthReturn
  result <- synthReturn(
    data = ret_two_evdates,
    unitname = "unit",
    treatname = "treat",
    dname = "date",
    rname = "ret",
    edname = "eventdate",
    estwind = c(-20, -1),
    eventwind = c(0, 2),
    ncores = 1,
    inference = "none"
  )

  expect_s3_class(result, "synthReturn")
  expect_s3_class(result$ar, "data.table")
  expect_true(all(c("ar", "sigma", "tau") %in% names(result$ar)))
})
