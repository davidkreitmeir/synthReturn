test_that("print.synthReturn displays output correctly", {
  # Test with actual synthReturn object
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
    ncores = 1,
    inference = "none"
  )

  # Capture the printed output
  output <- capture.output(print(result))

  # Check that output contains expected elements
  expect_true(any(grepl("Average Treatment Effect", output)))
  expect_true(length(output) > 3) # Should have multiple lines of output
})

test_that("print.synthReturn handles inference results", {
  # Test with permutation inference
  data("ret_two_evdates")

  result <- synthReturn(
    data = ret_two_evdates,
    unitname = "unit",
    treatname = "treat",
    dname = "date",
    rname = "ret",
    edname = "eventdate",
    estwind = c(-15, -1),
    eventwind = c(0, 1),
    inference = "permutation",
    ncores = 1,
    ndraws = 5  # Small number for testing
  )

  # Should not error when printing with inference
  expect_no_error(print(result))

  output <- capture.output(print(result))
  expect_true(any(grepl("Treatment Effect", output)))
  expect_true(length(output) > 0)
})
