test_that("phi_comp functionality tested through synthReturn", {
  skip_if_not_installed("parallel")
  skip_if_not_installed("mirai")

  # Test that phi_comp works through the main interface
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
  expect_s3_class(result$ate, "data.table")
  expect_true("phi" %in% names(result$ate))
  expect_true("tau" %in% names(result$ate))
})

test_that("phi_comp works with permutation inference", {
  skip_if_not_installed("parallel")
  skip_if_not_installed("mirai")

  data("ret_two_evdates")

  result <- synthReturn(
    data = ret_two_evdates,
    unitname = "unit",
    treatname = "treat",
    dname = "date",
    rname = "ret",
    edname = "eventdate",
    estwind = c(-20, -1),
    eventwind = c(0, 2),
    inference = "permutation",
    ncores = 1,
    ndraws = 5 # Small number for testing
  )

  expect_s3_class(result, "synthReturn")
  expect_false(is.null(result$ate_placebo))
  expect_true(result$n_placebo >= 0)
})
