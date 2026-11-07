#grants_cal <- readr::read_csv("C:/Users/Joe/pkgs/hpmicrosimr/inst/grant_calibration_data.csv")
#efficiency_cal <- readr::read_csv("C:/Users/Joe/pkgs/hpmicrosimr/inst/efficiency_calibration_data.csv")




#' @title calABM
#'
#' @description
#' macro-calibration of hpmicrosimr. Replicates run on a doParallel cluster (works on Windows) or serially.\cr
#' \cr
#' based on heat pump uptake, grant uptake and number of b2s on 1 nov 2025\cr
#' \cr
#' The output is the same for parallel and serial runs. See also calABM2, which uses forking on linux systems.
#'
#' @param sD base scenario (historical)
#' @param Nrun number of runs
#' @param n_unused_cores unsued cores default 2
#' @param use_parallel TRUE or FALSE
#' @param nu financial utility scale (drawn from financial_utility_scale)
#' @param p upgrade rate parameter default
#' @param r risk neutral rate
#' @param beta present bias
#' @param eta OSS sludge/hassle
#' @param tau sludge (transaction cost)
#' @param lambda hypothetical bias adjustment
#' @param rho rebound
#' @param quiet TRUE to suppress messages (default)
#'
#' @returns a list with components parameters (calibration targets), efficiency and grants
#' @export
#'
#' @examples
#'
#' #test <- calABM(sD,4,2,T)

calABM <- function(sD,Nrun=4,n_unused_cores=2,use_parallel=T,nu=0.4,p=0.004,r=0.03,beta=0.8,eta=0.02,tau=0.02,lambda=0,rho=0.3,quiet=TRUE){

  cal_values <- c("nu."=nu,"p."=p,"r."=r,"beta."=beta,"eta."=eta,"tau."=tau,"lambda."=lambda,"rho."=rho)
  run_calibration(sD, Nrun, backend = if(use_parallel) "foreach" else "serial", n_unused_cores, cal_values, quiet)
}


#' calABM2
#'
#' macro-calibration of hpmicrosimr using mclapply on linux systems (on Windows it falls back to calABM's backend)\cr
#'\cr
#' returns the same calibration data as calABM. The hypothetical bias parameter lambda. is left at its value in sD.
#'
#' @param sD base scenario (historical)
#' @param Nrun number of runs
#' @param n_unused_cores unsued cores default 2
#' @param use_parallel TRUE or FALSE
#' @param nu financial utility scale factor
#' @param p rate parameter
#' @param beta present bias
#' @param r finance rate
#' @param eta disruption
#' @param tau sludge
#' @param rho rebound
#' @param quiet TRUE to suppress messages (default)
#'
#' @returns a list with components parameters (calibration targets), efficiency and grants
#' @export
#'
#' @examples
#' #test <- calABM2(sD,1,use_parallel=F,nu=0.35,p=0.02,beta=0.8,r=0.05,eta=0.1,tau=0.02)

calABM2 <- function(sD, Nrun=4,n_unused_cores=2, use_parallel=T, nu=0.4,p=0.04,beta=0.8,r=0.03,eta=0.05,tau=0.02,rho=0.3,quiet=TRUE){

  cal_values <- c("nu."=nu,"p."=p,"r."=r,"beta."=beta,"eta."=eta,"tau."=tau,"rho."=rho)
  run_calibration(sD, Nrun, backend = if(use_parallel) "mclapply" else "serial", n_unused_cores, cal_values, quiet)
}

#calABM(sD,10,beta=params$beta.,lambda=params$lambda.,p=params$p.,nu=params$nu.,rho=params$rho.,r.=params$r.)

#' cal_summary
#'
#' Reduces ABM standard output to the macro-calibration targets: heat pump uptake, B2 counts, grant numbers and grant
#' costs between start_date and cal_date. Used by calABM and calABM2.
#'
#' @param abm ABM output, a dataframe with columns date and simulation (e.g. from run_replicates)
#' @param cal_params one row tibble of the calibration parameters, returned as the first columns of \code{parameters}
#' @param cal_date calibration date
#' @param start_date start of the calibration period
#'
#' @returns a list with components parameters (the calibration targets), efficiency and grants
#' @noRd
cal_summary <- function(abm, cal_params, cal_date = "2025-11-01", start_date = "2015-01-01"){

  cal_d <- lubridate::ymd(cal_date)
  start_d <- lubridate::ymd(start_date)
  cal_dates <- c(start_d, lubridate::ymd("2021-01-01"), cal_d) #two dates for calibration plus the start date

  test <- summarise_abm_cal2(abm, cal_date)

  heat_pumps <- test$tech %>% dplyr::filter(date %in% cal_dates, tech == "heat_pump") %>% dplyr::ungroup() %>%
    dplyr::select(date, n_heat_pump = n_tech)

  hp_grants <- test$grants %>% dplyr::filter(date %in% cal_dates) %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(n_heat_pump_grants = sum(n_hp_grant), .groups = "drop")

  # test$efficiency now also carries n_b2_grant_failure, n_b2_grant_upgrade,
  # n_b2_grant_fabric_only and n_b2_grant_upgrade_hp, so they pass straight through here
  efficiencies <- test$efficiency %>% dplyr::filter(date %in% cal_dates) %>% dplyr::ungroup() %>%
    dplyr::inner_join(heat_pumps, by = "date") %>%
    dplyr::left_join(hp_grants, by = "date") %>%
    dplyr::mutate(n_heat_pump_grants = tidyr::replace_na(n_heat_pump_grants, 0))
  grants <- test$grants %>% dplyr::filter(date %in% cal_dates)

  #a missing date or scheme would otherwise silently give a zero-row result, so insist on exactly one value
  one <- function(x, what){
    if(length(x) != 1) stop("cal_summary: expected one value for ", what, " but found ", length(x))
    x
  }
  hp_at <- function(d){
    x <- test$tech %>% dplyr::filter(date == d, tech == "heat_pump")
    one(x[["n_tech"]], paste("heat pumps at", d))
  }
  grant_at <- function(scheme, col){
    x <- test$grants %>% dplyr::filter(date == cal_d, grant_type == scheme)
    one(x[[col]], paste(col, "for", scheme))
  }
  eff_at <- function(d, col){
    x <- test$efficiency %>% dplyr::filter(date == d)
    one(x[[col]], paste(col, "at", d))
  }

  n_heat_pump <- hp_at(cal_d)
  n_heat_pump_0 <- hp_at(start_d)
  n_b2 <- eff_at(cal_d, "n_b2")           # total B2-or-better homes in the population
  n_b2_0 <- eff_at(start_d, "n_b2")
  #NB: n_fabric_total has always reported n_b2 at cal_date. The cumulative number of fabric upgrades is
  #n_upgrade below, which may be what was intended.
  n_fabric_total <- n_b2
  n_heat_pump_grant <- sum(test$grants$n_hp_grant[test$grants$date == cal_d])
  cals <- cal_params %>% dplyr::bind_cols(tibble::tibble(
    n_heat = n_heat_pump - n_heat_pump_0, n_heat_2015 = n_heat_pump_0,
    n_heat_pump_grant = n_heat_pump_grant,
    n_b2 = n_b2 - n_b2_0, number_b2_2015 = n_b2_0,
    n_b2_total = n_b2,
    # grant-assisted B2 crossings and their routes
    n_b2_grant             = eff_at(cal_d, "n_b2_grant"),
    n_b2_grant_failure     = eff_at(cal_d, "n_b2_grant_failure"),
    n_b2_grant_upgrade     = eff_at(cal_d, "n_b2_grant_upgrade"),
    n_b2_grant_fabric_only = eff_at(cal_d, "n_b2_grant_fabric_only"),
    n_b2_grant_upgrade_hp  = eff_at(cal_d, "n_b2_grant_upgrade_hp"),
    n_upgrade = eff_at(cal_d, "n_upgrade"),
    oss_total = grant_at("OSS", "grants_Meuro"),
    warmerhomes_total = grant_at("WarmerHomes", "grants_Meuro"),
    betterenergy_total = grant_at("BetterEnergyHomes", "grants_Meuro"),
    n_oss = grant_at("OSS", "n_all_grant"),
    n_warmerhomes = grant_at("WarmerHomes", "n_all_grant"),
    n_betterenergy = grant_at("BetterEnergyHomes", "n_all_grant"),
    n_fabric_total = n_fabric_total))

  list(parameters = cals, efficiency = efficiencies, grants = grants)
}



#' run_calibration
#'
#' Common engine of calABM and calABM2: sets the calibration parameters in the scenario, runs the 2015-2025
#' replicates on the chosen backend and summarises the calibration targets.
#'
#' @param sD base scenario (historical)
#' @param Nrun number of runs
#' @param backend "foreach", "mclapply" or "serial", see run_replicates
#' @param n_unused_cores unused cores
#' @param cal_values named numeric vector of calibration parameters e.g. c("nu."=0.35,"r."=0.04,"beta."=,"eta."=,"tau."="rho.=")
#' @param quiet TRUE to suppress messages
#'
#' @returns list(parameters, efficiency, grants) from calibration_summary
#' @noRd
run_calibration <- function(sD, Nrun, backend, n_unused_cores, cal_values, quiet = TRUE){

  year_zero <- 2015
  simulation_end <- 2025
  sD_cal <- sD
  for(nm in names(cal_values)){
    stopifnot(any(sD_cal$parameter == nm))
    sD_cal$value[sD_cal$parameter == nm] <- cal_values[[nm]]
  }
  print(paste(paste0(names(cal_values), "=", cal_values), collapse = ", "))

  seeds <- sample.int(.Machine$integer.max, Nrun)
  one_run <- function(j){
    set.seed(seeds[j])
    run_single(j, sD_cal, year_zero, simulation_end, resample_society = FALSE, ignore_social = FALSE, quiet = quiet)
  }
  abm <- run_replicates(Nrun, one_run, backend = backend, n_unused_cores = n_unused_cores)
  abm <- abm %>% dplyr::arrange(simulation, date)

  cal_params <- tibble::as_tibble(as.list(cal_values[c("nu.","p.","r.","beta.", "eta.", "tau.", "rho.")]))
  cal_summary(abm, cal_params)
}




#' Summarise ABM calibration outputs
#'
#' Summarises technology stocks, cumulative grants, and building efficiency.
#' Counts are scaled to the owner-occupied housing stock and averaged across
#' simulations, including simulations with zero qualifying households.
#'
#' @param abm Agent-level output from runABM() or calABM2()
#' @param cal_date Calibration date
#'
#' @return A list containing tech, grants, and efficiency dataframes.
#' @export
summarise_abm_cal2 <- function(abm, cal_date = "2025-11-01") {

  housing_stock_oo <- 1147552 # 2016 census
  cal_d <- lubridate::ymd(cal_date)

  technologies <- c(
    "oil", "gas", "electricity", "heat_pump", "wood"
  )

  schemes <- c(
    "BetterEnergyHomes", "OSS", "WarmerHomes"
  )

  # Return NA when a technology is absent from every simulation.
  # Otherwise retain missing values in means for populated groups.
  mean_present <- function(x, present) {
    if (!any(present)) {
      return(NA_real_)
    }
    mean(x[present])
  }

  # Simulation sizes at the calibration date.
  df0 <- abm %>%
    dplyr::filter(date == cal_d) %>%
    dplyr::group_by(simulation) %>%
    dplyr::summarise(
      n0 = dplyr::n(),
      .groups = "drop"
    )

  if (!setequal(df0$simulation, unique(abm$simulation))) {
    stop(
      "Every simulation must contain observations at cal_date: ",
      cal_date
    )
  }

  simulation_dates <- abm %>%
    dplyr::distinct(simulation, date)

  # ------------------------------------------------------------
  # Installed technologies
  # ------------------------------------------------------------

  ntech <- abm %>%
    dplyr::group_by(simulation, date, tech) %>%
    dplyr::summarise(
      n_tech = dplyr::n(),
      n_b2 = sum(ber < 125),
      ber = mean(ber),
      hli = mean(hli),
      .groups = "drop"
    ) %>%
    tidyr::complete(
      simulation,
      date,
      tech = technologies,
      fill = list(
        n_tech = 0L,
        n_b2 = 0L
      ),
      explicit = FALSE
    ) %>%
    dplyr::inner_join(df0, by = "simulation") %>%
    dplyr::mutate(
      present = n_tech > 0,
      n_tech = n_tech / n0 * housing_stock_oo,
      n_b2 = n_b2 / n0 * housing_stock_oo
    ) %>%
    dplyr::group_by(tech, date) %>%
    dplyr::summarise(
      n_tech = round(mean(n_tech)),
      ber = mean_present(ber, present),
      hli = mean_present(hli, present),
      n_b2 = round(mean(n_b2)),
      .groups = "drop"
    )

  # ------------------------------------------------------------
  # Cumulative grant amounts and counts
  # ------------------------------------------------------------

  grant_grid <- tidyr::expand_grid(
    simulation = unique(abm$simulation),
    date = unique(abm$date),
    grant_type = schemes
  )

  egrants <- abm %>%
    dplyr::arrange(simulation, serial, date) %>%
    dplyr::group_by(simulation, serial) %>%
    dplyr::mutate(ber_prev = dplyr::lag(ber, default = Inf)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(grant_type), grant_type != "None", date <= cal_d) %>%
    dplyr::group_by(simulation, grant_type, date) %>%
    dplyr::summarise(
      upgrade_grants   = sum(upgrade_grant),
      heat_pump_grants = sum(heat_pump_grant),
      n_all_grant    = sum(upgrade_grant > 0 | heat_pump_grant > 0, na.rm = TRUE),
      n_fabric_only_grant = sum(upgrade_grant > 0 & ((heat_pump_grant == 0) | is.na(heat_pump_grant)), na.rm = TRUE),
      n_b2_grant = sum((upgrade_grant > 0 | heat_pump_grant > 0) & ber < 125 & ber_prev >= 125, na.rm = TRUE),
      n_hp_grant = sum(heat_pump_grant > 0 & tech == "heat_pump", na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::full_join(grant_grid, by = c("simulation", "date", "grant_type")) %>%
    dplyr::mutate(dplyr::across(c(upgrade_grants, heat_pump_grants, n_all_grant, n_fabric_only_grant, n_b2_grant, n_hp_grant),
                                ~ tidyr::replace_na(.x, 0))) %>%
    dplyr::arrange(simulation, grant_type, date) %>%
    dplyr::group_by(simulation, grant_type) %>%
    dplyr::mutate(dplyr::across(c(upgrade_grants, heat_pump_grants, n_all_grant, n_fabric_only_grant,n_b2_grant, n_hp_grant), cumsum)) %>%
    dplyr::ungroup() %>%
    dplyr::inner_join(df0, by = "simulation") %>%
    dplyr::mutate(dplyr::across(c(upgrade_grants, heat_pump_grants, n_all_grant, n_b2_grant, n_hp_grant),
                                ~ .x / n0 * housing_stock_oo)) %>%
    dplyr::group_by(grant_type, date) %>%
    dplyr::summarise(dplyr::across(c(upgrade_grants, heat_pump_grants, n_all_grant, n_b2_grant, n_hp_grant), mean),
                     .groups = "drop") %>%
    dplyr::mutate(grants_Meuro = (upgrade_grants + heat_pump_grants) / 1e6) %>%
    dplyr::select(grant_type, date, upgrade_grants, heat_pump_grants, grants_Meuro,
                  n_all_grant, n_b2_grant, n_hp_grant)
  # ------------------------------------------------------------
  # Cumulative fabric-upgrade events, with or without grant support (lots of micro actions)
  # ------------------------------------------------------------

  nupgrades <- abm %>%
    dplyr::filter(
      date <= cal_d,
      upgrade
    ) %>%
    dplyr::group_by(simulation, date) %>%
    dplyr::summarise(
      n_upgrade = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::full_join(
      simulation_dates,
      by = c("simulation", "date")
    ) %>%
    dplyr::mutate(
      n_upgrade = tidyr::replace_na(n_upgrade, 0L)
    ) %>%
    dplyr::arrange(simulation, date) %>%
    dplyr::group_by(simulation) %>%
    dplyr::mutate(
      n_upgrade = cumsum(n_upgrade)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::inner_join(df0, by = "simulation") %>%
    dplyr::mutate(
      n_upgrade = n_upgrade / n0 * housing_stock_oo
    ) %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(
      n_upgrade = mean(n_upgrade),
      .groups = "drop"
    )

  # ------------------------------------------------------------
  # Building efficiency and B2-or-better stock.
  # eff$n_b2 is the total number of B2-or-better homes in the population
  # (stock, scaled to housing_stock_oo); n_b2_grant* are cumulative grant-assisted crossings.
  # ------------------------------------------------------------
  # ------------------------------------------------------------
  # Cumulative grant-assisted B2 events: a grant-assisted upgrade that takes a
  # home from above B2 (ber >= 125) to B2 or better (ber < 125).
  # Comparable to the SEAI count of grant-assisted B2s.
  # ------------------------------------------------------------
  # Splits grant-assisted B2 events by route:
  #   failure = heating-system failure replacement (upgrade == FALSE)
  #   upgrade = voluntary efficiency upgrade       (upgrade == TRUE)
  # Upgrades are split again by whether a heat pump was installed in that step
  # (tech is heat_pump now and was not in the previous record):
  #   fabric_only = upgrade, no heat pump installed (includes homes that already had one)
  #   upgrade_hp  = upgrade with a new heat pump installed
  b2_cols <- c("n_b2_grant", "n_b2_grant_failure", "n_b2_grant_upgrade",
               "n_b2_grant_fabric_only", "n_b2_grant_upgrade_hp")

  nb2_grant <- abm %>%
    dplyr::arrange(simulation, serial, date) %>%
    dplyr::group_by(simulation, serial) %>%
    dplyr::mutate(ber_prev = dplyr::lag(ber, default = Inf),
                  tech_prev = dplyr::lag(tech)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(
      date <= cal_d,
      !is.na(grant_type), grant_type != "None",
      upgrade_grant > 0 | heat_pump_grant > 0, ber < 125, ber_prev >= 125) %>%
    dplyr::mutate(hp_installed = tech == "heat_pump" & !dplyr::coalesce(tech_prev == "heat_pump", FALSE)) %>%
    dplyr::group_by(simulation, date) %>%
    dplyr::summarise(
      n_b2_grant         = dplyr::n(),
      n_b2_grant_failure = sum(!upgrade, na.rm = TRUE),
      n_b2_grant_upgrade = sum(upgrade, na.rm = TRUE),
      n_b2_grant_fabric_only = sum(upgrade & !hp_installed, na.rm = TRUE),
      n_b2_grant_upgrade_hp  = sum(upgrade & hp_installed, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::full_join(simulation_dates, by = c("simulation", "date")) %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(b2_cols), ~ tidyr::replace_na(.x, 0L))) %>%
    dplyr::arrange(simulation, date) %>%
    dplyr::group_by(simulation) %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(b2_cols), cumsum)) %>%
    dplyr::ungroup() %>%
    dplyr::inner_join(df0, by = "simulation") %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(b2_cols), ~ .x / n0 * housing_stock_oo)) %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(dplyr::across(dplyr::all_of(b2_cols), mean), .groups = "drop")

  eff <- abm %>%
    dplyr::group_by(simulation, date) %>%
    dplyr::summarise(
      n_b2 = sum(ber < 125),
      ber = mean(ber),
      hli = mean(hli),
      .groups = "drop"
    ) %>%
    dplyr::inner_join(df0, by = "simulation") %>%
    dplyr::mutate(
      n_b2 = n_b2 / n0 * housing_stock_oo
    ) %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(
      ber = round(mean(ber)),
      hli = mean(hli),
      n_b2 = round(mean(n_b2)),
      .groups = "drop"
    ) %>%
    dplyr::inner_join(nupgrades, by = "date") %>%
    dplyr::inner_join(nb2_grant, by = "date")

  return(list(
    tech = ntech,
    grants = egrants,
    efficiency = eff
  ))
}


#' cal_error
#'
#' \code{cal_error()} calculates the calibration error from the output of calABM() or calABM2(). The is used in the Bayesian optimsation
#' to find the optimal behavioural parameter set.
#'
#' \code{cal_error()} sums the mean square model fractional errors relative to observations (SEAI grant uptake over the period 2015=01-01 to 2025-11-01). The variable considered are the number grant-assisted of B2
#' upgrades since 2015, the number of of grant-assisted heat pumps installed since 2015, the number of WarmerHomes, OSS and BetterEnergyHomes grants, and the total expenditure in those
#' three schemes. To balance error in energy efficiency measure vs grants, the former error is multiplied by a factor of 2.5, so that the error is weighted
#' towards accuracy .
#'
#' @param abm_cal outpit of calABM2()
#'
#' @returns  a positive real number
#' @export
#'
#' @examples
cal_error <- function(abm_cal) {
  #
  cal_dates <- c("2025-11-01","2021-01-01")
  #df <- calABM(sD, Nrun=32, 2, TRUE, nu=x[1], p=x[2], beta=x[3],r=x[4], eta=x[5], tau=x[6], rho=0.3)
  df_eff <- abm_cal$efficiency %>% dplyr::inner_join(efficiency_cal, by="date")
  n_heat_pump0 <- df_eff %>% dplyr::filter(date=="2015-01-01") %>% dplyr::pull(n_heat_pump)
  n_b2_0 <- df_eff %>% dplyr::filter(date=="2015-01-01") %>% dplyr::pull(n_b2)
  df_eff <- df_eff %>% dplyr::filter(date %in% cal_dates) %>%
  dplyr::mutate(n_heat_pump_err = (n_heat_pump_grants - n_heat_pump_obsv)/n_heat_pump_obsv,
                  n_b2_err = (n_b2_grant - n_b2_obsv)/n_b2_obsv)

  df_grant <- abm_cal$grants %>% dplyr::inner_join(grants_cal)
  df_grant <- df_grant %>% dplyr::filter(date %in% cal_dates) %>%
    dplyr::mutate(n_error = (n_all_grant - n_obsv)/n_obsv,
           euro_error = (grants_Meuro - Meuro_obsv)/Meuro_obsv)

  err_2025 <- sum((df_grant %>% dplyr::filter(date=="2025-11-01") %>% dplyr::pull(n_error))^2) #mse relative number of grants
  err_2025 <- err_2025 + sum((df_grant %>% dplyr::filter(date=="2025-11-01") %>% dplyr::pull(euro_error))^2) #mse relative grant amount

  df_eff <- df_eff %>% dplyr::filter(date=="2025-11-01")
  err_2025 <- err_2025 + 2.5*sum(df_eff$n_heat_pump_err^2 + df_eff$n_b2_err^2) #give a heavier weighting to efficiency because there are three grant cateogries

  #print(paste("evaluated at nu. =", x[1], "p. = ", x[2], "beta.=", x[3],
  #            "r.=", x[4], "eta=", x[5], "tau=", x[6], "error=", err_2025))

  return(err_2025)
}

