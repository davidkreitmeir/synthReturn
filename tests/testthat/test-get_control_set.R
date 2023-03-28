devtools::load_all()

test_that("get_treat_set function works", {


  eds <- base::unique(r_treat[, .SD[1], by = tid]$ed)

  r_control <- purrr::map(
    eds,
    get_control_set,
    data = r_control,
    estwind = estwind,
    eventwind = eventwind,
    estobs_min = estobs_min,
    eventtobs_min = eventobs_min
  )

})


