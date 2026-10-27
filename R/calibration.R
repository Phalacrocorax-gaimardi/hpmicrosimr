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
#' #test <- calABM2(sD,2,2,F)

calABM2 <- function(sD, Nrun=4,n_unused_cores=2, use_parallel=T, nu=0.4,p=0.04,beta=0.8,r=0.03,eta=0.05,tau=0.02,rho=0.3,quiet=TRUE){

  cal_values <- c("nu."=nu,"p."=p,"r."=r,"beta."=beta,"eta."=eta,"tau."=tau,"rho."=rho)
  run_calibration(sD, Nrun, backend = if(use_parallel) "mclapply" else "serial", n_unused_cores, cal_values, quiet)
}

#calABM(sD,10,beta=params$beta.,lambda=params$lambda.,p=params$p.,nu=params$nu.,rho=params$rho.,r.=params$r.)

#' cal_summary
#'
#' Reduces ABM output to the macro-calibration targets: heat pump uptake, B2 counts, grant numbers and grant
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
  efficiencies <- test$efficiency %>% dplyr::filter(date %in% cal_dates) %>% dplyr::ungroup() %>%
    dplyr::inner_join(heat_pumps, by = "date")
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
  n_b2 <- eff_at(cal_d, "n_b2")
  n_b2_0 <- eff_at(start_d, "n_b2")
  #NB: n_fabric_total has always reported n_b2 at cal_date. The cumulative number of fabric upgrades is
  #eff_at(cal_d, "n_upgrade"), which may be what was intended.
  n_fabric_total <- n_b2

  cals <- cal_params %>% dplyr::bind_cols(tibble::tibble(
    n_heat = n_heat_pump - n_heat_pump_0, n_heat_2015 = n_heat_pump_0,
    number_b2 = n_b2 - n_b2_0, number_b2_2015 = n_b2_0,
    oss_total = grant_at("OSS", "grants_Meuro"),
    warmerhomes_total = grant_at("WarmerHomes", "grants_Meuro"),
    betterenergy_total = grant_at("BetterEnergyHomes", "grants_Meuro"),
    n_oss = grant_at("OSS", "n_grant"),
    n_warmerhomes = grant_at("WarmerHomes", "n_grant"),
    n_betterenergy = grant_at("BetterEnergyHomes", "n_grant"),
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




#analysis functions for ABM output

#' summarise_abm_cal
#'
#' A function to summarise the tech and energy efficiency adoption. Used for calibration but also general summaries of uptake.\cr
#' \cr
#' The summarise quantities are useful for calibration. A list of three dataframes is returned summarise by
#' (1) technology (2) grant totals and (c) building efficiency.
#'
#' @param abm runABM output
#' @param cal_date the calibration date
#'
#' @returns a list fo summary dataframes
#' @export
#'
#' @examples
#'
summarise_abm_cal <- function(abm, cal_date = "2025-11-01"){

  housing_stock_oo <- 1147552 #2016 census
  #print(housing_stock_oo)
  #simulation run sizes
  df0 <- abm %>% dplyr::filter(date==lubridate::ymd(cal_date)) %>% dplyr::group_by(simulation) %>% dplyr::summarise(n0=dplyr::n())
  ###############
  # installed tech
  ################
  ntech <-  abm %>% dplyr::group_by(simulation,date,tech) %>% dplyr::summarise(n_tech=dplyr::n(),n_b2=sum(ber < 125),ber=mean(ber), hli=mean(hli))
  ntech <- ntech %>% dplyr::inner_join(df0) %>% dplyr::mutate(n_tech=n_tech/n0*housing_stock_oo,n_b2=n_b2/n0*housing_stock_oo) %>% dplyr::select(-n0)
  ntech <- ntech %>% dplyr::group_by(tech,date) %>% dplyr::summarise(n_tech=round(mean(n_tech)),ber=mean(ber),hli=mean(hli),n_b2=round(mean(n_b2)))

  #########################
  #cumulative grants ( n & euro)
  #############################
  #
  egrants <-  abm %>% dplyr::filter(!is.na(grant_type), grant_type != "None", date <= lubridate::ymd(cal_date)) %>% dplyr::group_by(simulation,grant_type,date) %>% dplyr::summarise(upgrade_grants=sum(upgrade_grant), heat_pump_grants=sum(heat_pump_grant)) %>% dplyr::ungroup()
  df <- abm %>% dplyr::select(simulation,date) %>% dplyr::distinct()
  df1 <- tidyr::expand_grid(simulation=abm$simulation %>% unique(), date=abm$date %>% unique(), grant_type=c("BetterEnergyHomes","OSS","WarmerHomes"))
  egrants <- egrants %>% dplyr::full_join(df1) %>% dplyr::mutate(upgrade_grants=tidyr::replace_na(upgrade_grants,0),heat_pump_grants=tidyr::replace_na(heat_pump_grants,0))
  egrants <- egrants %>% dplyr::arrange(simulation,grant_type,date)
  #cumulative grants awarded
  egrants <- egrants %>% dplyr::group_by(simulation,grant_type) %>% dplyr::mutate(upgrade_grants=cumsum(upgrade_grants),heat_pump_grants=cumsum(heat_pump_grants))
  #scale up
  egrants <- egrants %>% dplyr::inner_join(df0) %>% dplyr::mutate(upgrade_grants=upgrade_grants/n0*housing_stock_oo,heat_pump_grants=heat_pump_grants/n0*housing_stock_oo) %>% dplyr::select(-n0)
  #mean over simulations
  egrants <- egrants %>% dplyr::group_by(grant_type,date) %>% dplyr::summarise(upgrade_grants=mean(upgrade_grants),heat_pump_grants=mean(heat_pump_grants))
  #
  egrants <- egrants %>% dplyr::mutate(grants_Meuro=(upgrade_grants+heat_pump_grants)/1e+6)
  ####################################################
  # cumulative number of grants awarded by scheme (measure blind)
  ####################################################
  ngrants <-  abm %>% dplyr::filter(!is.na(grant_type), grant_type != "None", date <= lubridate::ymd(cal_date))
  ngrants <- ngrants %>% dplyr::group_by(simulation,grant_type,date) %>% dplyr::summarise(n_grant=sum(upgrade_grant > 0 | heat_pump_grant > 0 ,na.rm=T)) %>% dplyr::ungroup()
  #df <- abm %>% dplyr::select(simulation,date) %>% dplyr::distinct()
  #df <- tidyr::expand_grid(simulation=abm$simulation %>% unique(), date=abm$date %>% unique(), grant_type=c("BetterEnergyHomes","OSS","WarmerHomes"))
  ngrants <-  ngrants %>% dplyr::full_join(df1) %>% dplyr::mutate(n_grant=tidyr::replace_na(n_grant,0))
  ngrants <-  ngrants %>% dplyr::arrange(simulation,grant_type,date)
  #cumulative total number
  ngrants <-  ngrants %>% dplyr::group_by(simulation,grant_type) %>% dplyr::mutate(n_grant=cumsum(n_grant))
  #scale up
  ngrants <-  ngrants %>% dplyr::inner_join(df0) %>% dplyr::mutate(n_grant=n_grant/n0*housing_stock_oo) %>% dplyr::select(-n0)
  #mean over simulations
  ngrants <-  ngrants %>% dplyr::group_by(grant_type,date) %>% dplyr::summarise(n_grant=mean(n_grant))

  egrants <- egrants %>% dplyr::inner_join(ngrants)

  ##############################################################################
  # fabric upgrades carried out irrespective of whether a grant was awarded or not
  #############################################################################

  nupgrades <-  abm %>% dplyr::filter(date <= lubridate::ymd(cal_date),upgrade)
  nupgrades <- nupgrades %>% dplyr::group_by(simulation,date) %>% dplyr::summarise(n_upgrade=dplyr::n()) %>% dplyr::ungroup()
  #df <- abm %>% dplyr::select(simulation,date) %>% dplyr::distinct()
  #df <- tidyr::expand_grid(simulation=abm$simulation %>% unique(), date=abm$date %>% unique(), grant_type=c("BetterEnergyHomes","OSS","WarmerHomes"))
  nupgrades <-  nupgrades %>% dplyr::full_join(df) %>% dplyr::mutate(n_upgrade=tidyr::replace_na(n_upgrade,0))
  nupgrades <-  nupgrades %>% dplyr::arrange(simulation,date)
  #cumulative total number
  nupgrades <-  nupgrades %>% dplyr::group_by(simulation) %>% dplyr::mutate(n_upgrade=cumsum(n_upgrade))
  #scale up
  nupgrades <-  nupgrades %>% dplyr::inner_join(df0) %>% dplyr::mutate(n_upgrade=n_upgrade/n0*housing_stock_oo) %>% dplyr::select(-n0)
  #mean over simulations
  nupgrades <-  nupgrades %>% dplyr::group_by(date) %>% dplyr::summarise(n_upgrade=mean(n_upgrade))

  ##################################################
  # efficiency; aggregate mean ber mean hli and n_b2
  ##################################################

  eff <-  abm %>% dplyr::group_by(simulation,date) %>% dplyr::summarise(ber=mean(ber), hli=mean(hli))
  eff1 <- abm %>% dplyr::filter(ber < 125) %>% dplyr::group_by(simulation,date) %>% dplyr::summarise(n_b2 = dplyr::n())
  eff <- eff %>% dplyr::inner_join(eff1) %>% dplyr::mutate(n_b2=tidyr::replace_na(n_b2,0))
  eff <- eff %>% dplyr::inner_join(df0) %>% dplyr::mutate(n_b2=n_b2/n0*housing_stock_oo) %>% dplyr::select(-n0)
  eff <- eff %>% dplyr::group_by(date) %>% dplyr::summarise(ber=round(mean(ber)),hli=mean(hli),n_b2=round(mean(n_b2)))

  eff <- eff %>% dplyr::inner_join(nupgrades)
  return(list("tech"=ntech,"grants"=egrants,"efficiency"=eff))

}


#' Summarise ABM calibration outputs
#'
#' Summarises technology stocks, cumulative grants, and building efficiency.
#' Counts are scaled to the owner-occupied housing stock and averaged across
#' simulations, including simulations with zero qualifying households.
#'
#' @param abm Agent-level output from runABM().
#' @param cal_date Calibration date used for population scaling and the
#'   cutoff for cumulative grant and fabric-upgrade events.
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
    dplyr::filter(
      !is.na(grant_type),
      grant_type != "None",
      date <= cal_d
    ) %>%
    dplyr::group_by(simulation, grant_type, date) %>%
    dplyr::summarise(
      upgrade_grants = sum(upgrade_grant),
      heat_pump_grants = sum(heat_pump_grant),
      n_grant = sum(
        upgrade_grant > 0 | heat_pump_grant > 0,
        na.rm = TRUE
      ),
      .groups = "drop"
    ) %>%
    dplyr::full_join(
      grant_grid,
      by = c("simulation", "date", "grant_type")
    ) %>%
    dplyr::mutate(
      upgrade_grants = tidyr::replace_na(upgrade_grants, 0),
      heat_pump_grants = tidyr::replace_na(heat_pump_grants, 0),
      n_grant = tidyr::replace_na(n_grant, 0L)
    ) %>%
    dplyr::arrange(simulation, grant_type, date) %>%
    dplyr::group_by(simulation, grant_type) %>%
    dplyr::mutate(
      upgrade_grants = cumsum(upgrade_grants),
      heat_pump_grants = cumsum(heat_pump_grants),
      n_grant = cumsum(n_grant)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::inner_join(df0, by = "simulation") %>%
    dplyr::mutate(
      upgrade_grants = upgrade_grants / n0 * housing_stock_oo,
      heat_pump_grants = heat_pump_grants / n0 * housing_stock_oo,
      n_grant = n_grant / n0 * housing_stock_oo
    ) %>%
    dplyr::group_by(grant_type, date) %>%
    dplyr::summarise(
      upgrade_grants = mean(upgrade_grants),
      heat_pump_grants = mean(heat_pump_grants),
      n_grant = mean(n_grant),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      grants_Meuro = (upgrade_grants + heat_pump_grants) / 1e6
    ) %>%
    dplyr::select(
      grant_type, date,
      upgrade_grants, heat_pump_grants,
      grants_Meuro, n_grant
    )

  # ------------------------------------------------------------
  # Cumulative fabric-upgrade events, with or without grants
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
  # Building efficiency and B2-or-better stock
  # ------------------------------------------------------------

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
    dplyr::inner_join(nupgrades, by = "date")

  return(list(
    tech = ntech,
    grants = egrants,
    efficiency = eff
  ))
}
#



cal_error <- function(abm_cal) {
  #
  cal_dates <- c("2025-11-01","2021-01-01")
  #df <- calABM(sD, Nrun=32, 2, TRUE, nu=x[1], p=x[2], beta=x[3],r=x[4], eta=x[5], tau=x[6], rho=0.3)
  df_eff <- abm_cal$efficiency %>% dplyr::inner_join(efficiency_cal, by="date")
  n_heat_pump0 <- df_eff %>% dplyr::filter(date=="2015-01-01") %>% dplyr::pull(n_heat_pump)
  n_b2_0 <- df_eff %>% dplyr::filter(date=="2015-01-01") %>% dplyr::pull(n_b2)
  df_eff <- df_eff %>% dplyr::filter(date %in% cal_dates) %>%
    dplyr::mutate(n_heat_pump_err = (n_heat_pump - n_heat_pump0 - n_heat_pump_obsv)/n_heat_pump_obsv,
           n_b2_err = (n_b2 - n_b2_0 - n_b2_obsv)/n_b2_obsv)

  df_grant <- abm_cal$grants %>% dplyr::inner_join(grants_cal)
  df_grant <- df_grant %>% dplyr::filter(date %in% cal_dates) %>%
    mutate(n_error = (n_grant - n_obsv)/n_obsv,
           euro_error = (grants_Meuro - Meuro_obsv)/Meuro_obsv)

  err_2025 <- sum((df_grant %>% filter(date=="2025-11-01") %>% pull(n_error))^2)
  err_2025 <- err_2025 + sum((df_grant %>% filter(date=="2025-11-01") %>% pull(euro_error))^2)

  df_eff <- df_eff %>% filter(date=="2025-11-01")
  err_2025 <- err_2025 + 3*sum(df_eff$n_heat_pump_err^2 + df_eff$n_b2_err^2)

  print(paste("evaluated at nu. =", x[1], "p. = ", x[2], "beta.=", x[3],
              "r.=", x[4], "eta=", x[5], "tau=", x[6], "error=", err_2025))

  return(err_2025)
}



