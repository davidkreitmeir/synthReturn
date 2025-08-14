test_that("get_treat_set and get_control_set work through synthReturn", {
  # These are internal functions, so we test them indirectly through synthReturn
  data("ret_two_evdates")

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

  expect_s3_class(result, "synthReturn")
  expect_true(result$n_treat_res > 0) # Should have processed some treatment units
  expect_s3_class(result$ar, "data.table")
  expect_true(nrow(result$ar) > 0) # Should have some abnormal returns
})

test_that("data processing handles missing observations", {
  data("ret_two_evdates")

  # Test with more lenient missing data requirements
  result <- synthReturn(
    data = ret_two_evdates,
    unitname = "unit",
    treatname = "treat",
    dname = "date",
    rname = "ret",
    edname = "eventdate",
    estwind = c(-50, -1),
    eventwind = c(0, 5),
    estobs_min = 0.8,  # Allow 20% missing in estimation window
    eventobs_min = 0.8, # Allow 20% missing in event window
    ncores = 1,
    inference = "none"
  )

  expect_s3_class(result, "synthReturn")
  expect_true(result$n_treat_res >= 0) # Should handle the data without errors
})
