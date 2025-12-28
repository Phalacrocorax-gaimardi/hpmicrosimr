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
  ntech <-  abm %>% dplyr::group_by(simulation,date,tech) %>% dplyr::summarise(n_tech=dplyr::n(),ber=mean(ber), hli=mean(hli),n_b2=sum(ber < 125))
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
  eff1 <- abm %>% dplyr::filter(ber <= 125) %>% dplyr::group_by(simulation,date) %>% dplyr::summarise(n_b2 = dplyr::n())
  eff <- eff %>% dplyr::inner_join(eff1) %>% dplyr::mutate(n_b2=tidyr::replace_na(n_b2,0))
  eff <- eff %>% dplyr::inner_join(df0) %>% dplyr::mutate(n_b2=n_b2/n0*housing_stock_oo) %>% dplyr::select(-n0)
  eff <- eff %>% dplyr::group_by(date) %>% dplyr::summarise(ber=round(mean(ber)),hli=mean(hli),n_b2=round(mean(n_b2)))

  eff <- eff %>% dplyr::inner_join(nupgrades)
  return(list("tech"=ntech,"grants"=egrants,"efficiency"=eff))

}
