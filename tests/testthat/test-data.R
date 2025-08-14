test_that("package data sets load correctly", {
  # Test ret_two_evdates
  data("ret_two_evdates", envir = environment())
  expect_true(exists("ret_two_evdates"))
  expect_s3_class(ret_two_evdates, "data.frame")

  # Check expected columns
  expected_cols <- c("eventdate", "date", "ret", "treat", "unit")
  expect_true(all(expected_cols %in% names(ret_two_evdates)))

  # Check data types
  expect_s3_class(ret_two_evdates$eventdate, "Date")
  expect_s3_class(ret_two_evdates$date, "Date")
  expect_type(ret_two_evdates$ret, "double")
  expect_type(ret_two_evdates$treat, "logical")
  expect_type(ret_two_evdates$unit, "integer")

  # Check that we have both treated and control units
  expect_true(any(ret_two_evdates$treat))
  expect_true(any(!ret_two_evdates$treat))
})

test_that("numeric date dataset loads correctly", {
  # Test ret_two_evdates_num
  data("ret_two_evdates_num", envir = environment())
  expect_true(exists("ret_two_evdates_num"))
  expect_s3_class(ret_two_evdates_num, "data.frame")

  # Check expected columns
  expected_cols <- c("eventdate", "date", "ret", "treat", "unit")
  expect_true(all(expected_cols %in% names(ret_two_evdates_num)))

  # Check data types - dates should be numeric in this dataset
  expect_type(ret_two_evdates_num$eventdate, "integer")
  expect_type(ret_two_evdates_num$date, "integer")
  expect_type(ret_two_evdates_num$ret, "double")
  expect_type(ret_two_evdates_num$treat, "logical")
  expect_type(ret_two_evdates_num$unit, "integer")
})

test_that("datasets have reasonable structure", {
  data("ret_two_evdates", envir = environment())

  # Check data dimensions
  expect_true(nrow(ret_two_evdates) > 1000) # Should have substantial data
  expect_equal(ncol(ret_two_evdates), 5)

  # Check that returns are reasonable (not all zero, not extremely large)
  expect_true(var(ret_two_evdates$ret, na.rm = TRUE) > 0)
  expect_true(all(abs(ret_two_evdates$ret) < 10, na.rm = TRUE)) # Returns should be reasonable

  # Check that we have multiple event dates
  event_dates <- unique(ret_two_evdates$eventdate[ret_two_evdates$treat])
  expect_true(length(event_dates) >= 1)

  # Check that we have multiple units
  n_units <- length(unique(ret_two_evdates$unit))
  expect_true(n_units > 10) # Should have multiple firms
})

test_that("datasets work with synthReturn function", {
  data("ret_two_evdates", envir = environment())

  # This is an integration test to ensure the data works with the main function
  expect_no_error({
    result <- synthReturn(
      data = ret_two_evdates,
      unitname = "unit",
      treatname = "treat",
      dname = "date",
      rname = "ret",
      edname = "eventdate",
      estwind = c(-20, -1),
      eventwind = c(0, 2),
      estobs_min = 1,
      eventobs_min = 1,
      inference = "none",
      ndraws = 5,
      ncores = 1
    )
  })
})
