test_that("event_panel functionality tested through synthReturn", {
  # Since event_panel is an internal function, we test it indirectly
  data("ret_two_evdates")

  # Test that the complete pipeline works (which uses event_panel internally)
  result <- synthReturn(
    data = ret_two_evdates,
    unitname = "unit",
    treatname = "treat",
    dname = "date",
    rname = "ret",
    edname = "eventdate",
    estwind = c(-20, -1),
    eventwind = c(0, 2),
    inference = "none",
    ncores = 1
  )

  expect_s3_class(result, "synthReturn")
  expect_s3_class(result$ar, "data.table")

  # Check that abnormal returns are computed (event_panel worked)
  expect_true("ar" %in% names(result$ar))
  expect_true("sigma" %in% names(result$ar))
})

test_that("event_panel handles different data scenarios", {
  data("ret_two_evdates")

  # Test with relaxed missing data requirements
  result <- synthReturn(
    data = ret_two_evdates,
    unitname = "unit",
    treatname = "treat",
    dname = "date",
    rname = "ret",
    edname = "eventdate",
    estwind = c(-30, -1),
    eventwind = c(0, 3),
    estobs_min = 0.8, # Allow some missing data
    eventobs_min = 0.8,
    inference = "none",
    ncores = 1
  )

  expect_s3_class(result, "synthReturn")
  expect_true(nrow(result$ar) > 0) # Should have some results
})
