## Simulate data

# Sample size of treated corporation by event-date
n <- 10L
# Sample size of control companies
nc <- 60L
# length of estimation window
estwindlen <- 100L
# length of event window
eventwindlen <- 6L

# vector containing start dates for each event panel (dmy format)
start_dates <- c("01-06-2020", "01-09-2020")

# Functions ---------------------------------------------------------------------------

sim_treat_returns <- function(start_date, estwindlen, eventwindlen){

  ret_treat_DT = data.table::data.table(
    date = seq(lubridate::dmy(start_date), by = "days", length.out = (estwindlen + eventwindlen)),
    ret = c(stats::rnorm(estwindlen, 0, 1), stats::rnorm(eventwindlen, 1, 1)),
    estwind = c(rep.int(1L, estwindlen), rep.int(0L, eventwindlen)),
    eventwind = c(rep.int(0L, estwindlen), rep.int(1L, eventwindlen)),
    eventdate = lubridate::dmy(start_date) + lubridate::days(estwindlen)
  )

  return(ret_treat_DT)
}

sim_control_ret <- function(start_date, end_date) {

  # sample period length
  sampleperiodlen = as.numeric(end_date - start_date + 1)

  ret_control_DT = data.table::data.table(
    date = seq(start_date, by = "days", length.out = sampleperiodlen),
    ret = stats::rnorm(sampleperiodlen, 0, 1)
  )

  return(ret_control_DT)
}

# Simulate Returns --------------------------------------------------------------------

set.seed(73101)

# Generate return data for company x event panels
ret_treat <- data.table::rbindlist(
  list(
    # 1st company x event panel
    data.table::rbindlist(
      purrr::map(
        rep.int(start_dates[1L], n),
        sim_treat_returns,
        estwindlen = estwindlen,
        eventwindlen = eventwindlen
      ),
      idcol = "tempid"
    ),
    # 2nd company x event panel
    data.table::rbindlist(
      purrr::map(
        rep(start_dates[2], n),
        sim_treat_returns,
        estwindlen = estwindlen,
        eventwindlen = eventwindlen
      ),
      idcol = "tempid"
    )
  )
)

# create unique event x firm id
ret_treat <- ret_treat[, treatid:=.GRP, by= .(tempid,eventdate)][, tempid := NULL]

# get time span of entire sample period
sampleperiod <- ret_treat[, .(min_date = min(date), max_date = max(date))]

# control company panel (nc control companies)
ret_control <- data.table::rbindlist(
  purr::map2(
    rep(sampleperiod$min_date, nc),
    rep(sampleperiod$max_date, nc),
    sim_control_ret
  ),
  idcol = "controlid"
)

# CREATE DATASETS ---------------------------------------------------------------------


# ONE EVENT DATE ----------------------------------------------------------------------

# Simplest Panel Structure
eventdates <- ret_treat[1L, "eventdate"][["eventdate"]]

ret_treat_one <- ret_treat[eventdate == eventdates,]
sampleperiod_one <- ret_treat_one[, .(min_date = min(date), max_date = max(date))]
ret_control_one <- ret_control[date %between% c(sampleperiod_one[["min_date"]], sampleperiod_one[["max_date"]]),]
ret_one_evdate = rbindlist(
  list(ret_treat_one, ret_control_one), fill = TRUE
)

usethis::use_data(ret_one_evdate, overwrite = TRUE)

# MULTIPLE (TWO) EVENT DATES ----------------------------------------------------------

ret_two_evdates = data.table::rbindlist(
  list(ret_treat, ret_control), fill = TRUE
)

usethis::use_data(ret_two_evdates, overwrite = TRUE)

# MULTIPLE (TWO) EVENT DATES + MISSING VALUES -----------------------------------------

# Randomly introduce missing values for 5\% of the return data
ret_two_evdates_na <- data.table::copy(ret_two_evdates)
ret_two_evdates_na[sample.int(nrow(ret_two_evdates_na), as.integer(nrow(ret_two_evdates_na) * 0.05)), ret := NA_real_]

usethis::use_data(ret_two_evdates_na, overwrite = TRUE)
