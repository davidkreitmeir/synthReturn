###################################################################################
## Function to compute the benchmark cumulative treatment effect
###################################################################################


phi_comp <- function(r_treat, r_control, r_treat_ed, estwind, eventwind, ncores, static_scheduling, is_windows, inference) {

  # r_treat: list of unit-specific data tables; columns: d, r; sorted by d
  # r_control: list of ed-specific data tables; columns: unit_id, d, r; sorted by unit_id, d; list elements are named according to ed value
  # r_treat_unit_ed: data table of event date per treated unit; columns: ed, unit_id; sorted by unit_id

  if(ncores == 1L) {
    # obtain event panel for each treatment group
    # compute abnormal returns (ARs) for each placebo treatment group firm
    ARs <- mapply(
      function(dt_treat, treat_ed, dt_control, estwind, eventwind) {
        event_panel(dt_treat, dt_control[[treat_ed]], estwind, eventwind)
      },
      dt_treat = r_treat,
      treat_ed = r_treat_ed,
      MoreArgs = list(
        dt_control = r_control,
        estwind = estwind,
        eventwind = eventwind
      ),
      SIMPLIFY = FALSE,
      USE.NAMES = FALSE
    )
  } else {
    if(is_windows) {
      if(inference != "none") {
        r_control <- r_control[r_treat_ed]
      }
      cl <- mirai::make_cluster(ncores)
      ARs <- parallel::clusterMap(
        cl,
        event_panel,
        dt_treat = r_treat,
        r_control = r_control,
        MoreArgs = list(
          estwind = estwind,
          eventwind = eventwind
        ),
        USE.NAMES = FALSE,
        .scheduling = data.table::fifelse(static_scheduling, "static", "dynamic")
      )
      mirai::stop_cluster(cl)
    } else {
      ARs <- parallel::mcmapply(
        function(dt_treat, treat_ed, dt_control, estwind, eventwind) {
          event_panel(dt_treat, dt_control[[treat_ed]], estwind, eventwind)
        },
        dt_treat = r_treat,
        treat_ed = r_treat_ed,
        MoreArgs = list(
          dt_control = r_control,
          estwind = estwind,
          eventwind = eventwind
        ),
        SIMPLIFY = FALSE,
        USE.NAMES = FALSE,
        mc.cores = ncores,
        mc.preschedule = static_scheduling
      )
    }
  }
  # get number of treatment units with synthetic matches
  n_treat_res <- sum(!vapply(ARs, is.null, logical(1L), USE.NAMES = FALSE), na.rm = TRUE)

  if(n_treat_res == 0L) {
    stop("phi could not be computed. Make sure that (i) there are control units observed on all observed days of treatment units and (ii) their returns ",
      "vary over time.")
  }

  ARs <- data.table::rbindlist(ARs)

  # compute phi - equ. (7)
  phi <- ARs[, .(phi = sum(car_wgted) / sum(one_div_sigma)), by = "tau"]
  out <- list(phi = phi, ar = ARs, n_treat_res = n_treat_res)

  return(out)
}

