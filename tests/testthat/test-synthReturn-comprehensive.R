test_that("synthReturn works with basic input parameters", {
  # Load test data
  data("ret_two_evdates")

  # Test basic functionality
  result <- synthReturn(
    data = ret_two_evdates,
    unitname = "unit",
    treatname = "treat",
    dname = "date",
    rname = "ret",
    edname = "eventdate",
    estwind = c(-50, -1),
    eventwind = c(0, 3),
    estobs_min = 1,
    eventobs_min = 1,
    inference = "none",
    ndraws = 10,
    ncores = 1
  )

  # Check that result has expected structure
  expect_s3_class(result, "synthReturn")
  expect_type(result, "list")
  expect_named(result, c("n_treat_pre", "n_treat_res", "ate", "ar", "call"))

  # Check ate component
  expect_s3_class(result$ate, "data.table")
  expect_true("tau" %in% names(result$ate))
  expect_true("phi" %in% names(result$ate))

  # Check ar component
  expect_s3_class(result$ar, "data.table")
  expect_true(all(c("ar", "sigma", "tau") %in% names(result$ar)))

  # Check that placebo is NULL when inference = "none"
  expect_null(result$ate_placebo)
  expect_null(result$n_placebo)
})

test_that("synthReturn works with placebo inference", {
  # Load test data
  data("ret_two_evdates")

  # Test with placebo inference
  result <- synthReturn(
    data = ret_two_evdates,
    unitname = "unit",
    treatname = "treat",
    dname = "date",
    rname = "ret",
    edname = "eventdate",
    estwind = c(-50, -1),
    eventwind = c(0, 3),
    estobs_min = 1,
    eventobs_min = 1,
    inference = "permutation",
    ndraws = 10,
    ncores = 1
  )

  # Check that placebo results are included
  expect_s3_class(result, "synthReturn")
  expect_false(is.null(result$ate_placebo))
  expect_false(is.null(result$n_placebo))

  # Check confidence intervals are added
  expect_true("ci_95_lower" %in% names(result$ate))
  expect_true("ci_95_upper" %in% names(result$ate))
  expect_true("pval" %in% names(result$ate))
})

test_that("synthReturn works with numeric dates", {
  # Load numeric date data
  data("ret_two_evdates_num")

  # Test with numeric dates
  result <- synthReturn(
    data = ret_two_evdates_num,
    unitname = "unit",
    treatname = "treat",
    dname = "date",
    rname = "ret",
    edname = "eventdate",
    estwind = c(-50, -1),
    eventwind = c(0, 3),
    estobs_min = 1,
    eventobs_min = 1,
    inference = "none",
    ndraws = 10,
    ncores = 1
  )

  expect_s3_class(result, "synthReturn")
  expect_s3_class(result$ate, "data.table")
  expect_s3_class(result$ar, "data.table")
})

test_that("synthReturn handles missing observations correctly", {
  # Load test data
  data("ret_two_evdates")

  # Test with relaxed missing data requirements
  result <- synthReturn(
    data = ret_two_evdates,
    unitname = "unit",
    treatname = "treat",
    dname = "date",
    rname = "ret",
    edname = "eventdate",
    estwind = c(-50, -1),
    eventwind = c(0, 3),
    estobs_min = 0.8,  # Allow 20% missing
    eventobs_min = 0.8, # Allow 20% missing
    inference = "none",
    ndraws = 10,
    ncores = 1
  )

  expect_s3_class(result, "synthReturn")
  expect_s3_class(result$ate, "data.table")
  expect_s3_class(result$ar, "data.table")
})

test_that("synthReturn validates input parameters", {
  data("ret_two_evdates")

  # Test missing required parameter
  expect_error(
    synthReturn(
      data = ret_two_evdates,
      # missing unitname
      treatname = "treat",
      dname = "date",
      rname = "ret",
      edname = "eventdate",
      estwind = c(-50, -1),
      eventwind = c(0, 3),
      ncores = 1
    )
  )

  # Test invalid column name
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
      ncores = 1
    )
  )

  # Test invalid window format
  expect_error(
    synthReturn(
      data = ret_two_evdates,
      unitname = "unit",
      treatname = "treat",
      dname = "date",
      rname = "ret",
      edname = "eventdate",
      estwind = c(-50), # Should be length 2
      eventwind = c(0, 3),
      ncores = 1
    ),
    "is not of format"
  )
})

test_that("synthReturn argument preservation works", {
  data("ret_two_evdates")

  result <- synthReturn(
    data = ret_two_evdates,
    unitname = "unit",
    treatname = "treat",
    dname = "date",
    rname = "ret",
    edname = "eventdate",
    estwind = c(-50, -1),
    eventwind = c(0, 3),
    estobs_min = 0.9,
    eventobs_min = 0.8,
    inference = "none",
    ndraws = 15,
    ncores = 1
  )

  # Check that arguments are preserved in call (need to eval them)
  expect_equal(eval(result$call$estwind), c(-50, -1))
  expect_equal(eval(result$call$eventwind), c(0, 3))
  expect_equal(eval(result$call$estobs_min), 0.9)
  expect_equal(eval(result$call$eventobs_min), 0.8)
  expect_equal(eval(result$call$ndraws), 15)
})
