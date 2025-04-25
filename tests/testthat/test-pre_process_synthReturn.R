test_that("1 event, no missing returns", {

  data <- ret_one_evdate

  expect_no_error(
    pre_process_synthReturn(
      data = data,
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
  )

})

test_that("2 events, no missing returns", {

  data <- ret_two_evdates

  expect_no_error(
    pre_process_synthReturn(
      data = data,
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
  )

})

test_that("2 events, missing returns", {

  data <- ret_two_evdates_na

  expect_no_error(
    pre_process_synthReturn(
      data = data,
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
  )

  expect_no_error(
    pre_process_synthReturn(
      data = data,
      tidname = "treatid",
      cidname = "controlid",
      dname = "date",
      rname = "ret",
      edname = "eventdate",
      estwind = c(-100, -1),
      eventwind = c(0, -5),
      estobs_min = 0.8,
      eventobs_min = 0.8,
      placebo = TRUE,
      ndraws = 25,
      ngroup = 2,
      parallel = FALSE,
      ncore = 1
    )
  )

  expect_error(
    pre_process_synthReturn(
      data = data,
      tidname = "treatid",
      cidname = "controlid",
      dname = "date",
      rname = "ret",
      edname = "eventdate",
      estwind = c(-100, -1),
      eventwind = c(0, -5),
      estobs_min = 200,
      eventobs_min = 10,
      placebo = TRUE,
      ndraws = 25,
      ngroup = 2,
      parallel = FALSE,
      ncore = 1
    ),
    "The value of"
  )


})



