test_that("pre_process_synthReturn functionality tested through synthReturn", {
  # Since pre_process_synthReturn is internal, test it through the main function
  data("ret_two_evdates")

  # Test that validation works - invalid column name should error
  expect_error(
    synthReturn(
      data = ret_two_evdates,
      unitname = "invalid_column",
      treatname = "treat",
      dname = "date",
      rname = "ret",
      edname = "eventdate",
      estwind = c(-50, -1),
      eventwind = c(0, 3),
      ncores = 1,
      inference = "none"
    )
  )

  # Test that valid data works
  expect_no_error({
    result <- synthReturn(
      data = ret_two_evdates,
      unitname = "unit",
      treatname = "treat",
      dname = "date",
      rname = "ret",
      edname = "eventdate",
      estwind = c(-30, -1),
      eventwind = c(0, 3),
      ncores = 1,
      inference = "none"
    )
  })
})

test_that("synthReturn handles different missing data requirements", {
  data("ret_two_evdates")

  # Test with proportional obs_min
  result <- synthReturn(
    data = ret_two_evdates,
    unitname = "unit",
    treatname = "treat",
    dname = "date",
    rname = "ret",
    edname = "eventdate",
    estwind = c(-50, -1), # 50 days
    eventwind = c(0, 5),   # 6 days
    estobs_min = 0.8,      # 80% of 50 = 40 days
    eventobs_min = 0.5,    # 50% of 6 = 3 days
    ncores = 1,
    inference = "none"
  )

  expect_s3_class(result, "synthReturn")
})

test_that("synthReturn validates inference parameters", {
  data("ret_two_evdates")

  # Test invalid inference method
  expect_error(
    synthReturn(
      data = ret_two_evdates,
      unitname = "unit",
      treatname = "treat",
      dname = "date",
      rname = "ret",
      edname = "eventdate",
      estwind = c(-20, -1),
      eventwind = c(0, 3),
      ncores = 1,
      inference = "invalid_method"
    )
  )
})
