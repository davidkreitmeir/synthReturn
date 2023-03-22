# create data set for 10 treated corporations with estimation window of 100 and event window of 6 days
# set.seed(73101)

sim_treat_returns <- function(start_date, estwindlen = 100, eventwindlen = 5){

  ret_treat_DT = data.table(
    date = seq(lubridate::dmy(start_date), by = "days", length.out = (estwindlen + eventwindlen)),
    ret = c(rnorm(estwindlen ,0, 1), rnorm(eventwindlen ,1, 1)),
    estwind = c(rep.int(1, estwindlen), rep.int(0, eventwindlen)),
    eventwind = c(rep.int(0, estwindlen), rep.int(1, eventwindlen)),
    eventdate = lubridate::dmy(start_date) + lubridate::days(estwindlen)
  )

  return(ret_treat_DT)

}

sim_control_ret <- function(start_date, end_date){

  # sample period length
  sampleperiodlen = as.numeric(end_date - start_date + 1)

  ret_control_DT = data.table(
    date = seq(start_date, by = "days", length.out = sampleperiodlen),
    ret = c(rnorm(sampleperiodlen, 0, 1))
  )

  return(ret_control_DT)

}

# Simulate Returns --------------------------------------------------------------------

ret_treat <- data.table::rbindlist(
  list(
    # 1st company x event panel
    data.table::rbindlist(
      map(
        rep("01-06-2020", 10),
        sim_treat_returns
      )
    ),
    # 2nd company x event panel
    data.table::rbindlist(
      map(
        rep("01-09-2020", 10),
        sim_treat_returns
      )
    )
  ),
  idcol = "treatid"
)

# get time span of entire sample period
sampleperiod <- ret_treat[, .(min_date = min(date), max_date = max(date))]

# control company panel (60 control companies)
ret_control <- data.table::rbindlist(
      map2(
        rep(sampleperiod$min_date, 60),
        rep(sampleperiod$max_date, 60),
        sim_control_ret
      ),
      idcol = "controlid"
    )

# CREATE DATASETS ---------------------------------------------------------------------


# ONE EVENT DATE ----------------------------------------------------------------------

# Simplest Panel Structure
eventdates <- unique(ret_treat[, "eventdate"]$eventdate)

ret_treat_one <-ret_treat[eventdate == eventdates[1],]
sampleperiod_one <- ret_treat_one[, .(min_date = min(date), max_date = max(date))]
ret_control_one <- ret_control[date >= sampleperiod_one$min_date  & date <= sampleperiod_one$max_date, ]
ret_one_evdate = rbindlist(
  list(ret_treat_one, ret_control_one), fill = TRUE
)

usethis::use_data(ret_one_evdate)

# MULTIPLE (TWO) EVENT DATES ----------------------------------------------------------

ret_two_evdates = rbindlist(
  list(ret_treat, ret_control), fill = TRUE
)

usethis::use_data(ret_two_evdates)


# MULTIPLE (TWO) EVENT DATES + MISSING VALUES -----------------------------------------

# Randomly introduce missing values for 5\% of the return data
ret_two_evdates_na <- ret_two_evdates[, ret := missForest::prodNA(ret_two_evdates[, "ret"], 0.05)]

usethis::use_data(ret_two_evdates_na)







