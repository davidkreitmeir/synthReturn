devtools::load_all()

test_that("pre_process function works", {

  pre_process_synthReturn(
    data = ret_one_evdate,
    tidname = "treatid",
    cidname = "controlid",
    dname = "date",
    rname = "ret",
    edname = "eventdate",
    estwind = c(-100, -1),
    eventwind = c(0, -5),
    estobs_min = 1,
    eventobs_min = 1,
    placebo = TRUE,
    ndraws = 25,
    ngroup = 2,
    parallel = FALSE,
    ncore = 1
  )

})
