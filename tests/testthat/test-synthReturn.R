# tweak the synthReturn() function and re-load it
devtools::load_all()

test_that("synthReturn works for 1 event date, no missing values and 1 data frame", {

  # load example data
  data("ret_one_evdate")

  synthReturn(
    data = ret_one_evdate,
    tidname = treatid,
    cidname = controlid,
    dname = date,
    rname = ret,
    estwind = c(-100, -1),
    eventwind = c(0,5)
  )
})
