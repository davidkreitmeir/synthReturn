devtools::load_all()

test_that("get_treat_set function works", {

  r_treat <- furrr::future_map(
    eds,
    get_treat_set,
    data = r_treat,
    estwind = estwind,
    eventwind = eventwind,
    estobs_min = estobs_min,
    eventtobs_min = eventobs_min
  )

})

