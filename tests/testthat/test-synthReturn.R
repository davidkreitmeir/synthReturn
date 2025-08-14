test_that("synthReturn works for 2 event dates, no missing values", {
  # load example data
  data("ret_two_evdates")

  synthReturn(
    data = ret_two_evdates,
    unitname = "unit",
    treatname = "treat",
    dname = "date",
    rname = "ret",
    edname = "eventdate",
    estwind = c(-50, -1),
    eventwind = c(0, 3),
    inference = "none",
    ncores = 1
  )
})
