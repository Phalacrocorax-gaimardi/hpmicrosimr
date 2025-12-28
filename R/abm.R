#hp_model_weights_oo <- readr::read_csv("C:/Users/joe/pkgs/hpmicrosimr/inst/extdata/hp_model_weights.csv")
#hp_empirical_utils <- readr::read_csv("C:/Users/joe/pkgs/hpmicrosimr/inst/extdata/hp_empirical_utils.csv")
#hp_survey_oo <- readr::read_csv("C:/Users/joe/pkgs/hpmicrosimr/inst/extdata/hp_survey_oo.csv")
#hp_survey_oo <- hp_survey_oo %>% dplyr::filter(!is.na(q53_5))
#hp_questions <- readr::read_csv("C:/Users/joe/pkgs/hpmicrosimr/inst/extdata/hp_questions.csv")
#hp_qanda <- readr::read_csv("C:/Users/joe/pkgs/hpmicrosimr/inst/extdata/hp_qanda.csv")
#sD <- readxl::read_xlsx("C:/Users/joe/pkgs/hpmicrosimr/inst/extdata/scenario_parameters.xlsx",sheet="WEM")


#' scenario_params
#'
#' builds the complete parameter set at yeartime from scenario sD
#'
#' @param sD scenario parameters e.g. scenario_0
#' @param yeartime decimal time
#'
#' @return long form dataframe containing parameter names and values
#' @export
#'
#' @examples scenario_params(sD,2025.5)
scenario_params <- function(sD,yeartime){
  #fast params
  technologies <- c("oil","gas","electricity","heat_pump","solid_fuel")
  scen <- tibble::tibble(parameter="yeartime", value=  yeartime)
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="hdd", value=  dplyr::filter(sD, parameter=="hdd")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="reference temperature", value=  dplyr::filter(sD, parameter=="")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="labour_cost", value=  labour_cost_fun(sD,yeartime)))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="heating_controller_cost", value=  dplyr::filter(sD, parameter=="heating_controller_cost")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="installer_overhead",  value=dplyr::filter(sD, parameter=="overhead")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="installer_profit_margin", value=  dplyr::filter(sD, parameter=="installer_profit_margin")$value))
  #system lifetimes and weibull shape parameter
  for(tech in technologies){
    scen <- dplyr::bind_rows(scen,tibble::tibble(parameter=paste(tech,"_system_lifetime",sep=""), value=  dplyr::filter(sD, parameter==paste(tech,"_system_lifetime",sep=""))$value))
    scen <- dplyr::bind_rows(scen,tibble::tibble(parameter=paste(tech,"_system_beta",sep=""), value=  dplyr::filter(sD, parameter==paste(tech,"_system_beta",sep=""))$value))

    }
  #system efficiencies
  for(tech in c("solid_fuel","electricity")){
    scen <- dplyr::bind_rows(scen,tibble::tibble(parameter=paste(tech,"_system_efficiency",sep=""), value=  dplyr::filter(sD, parameter==paste(tech,"_system_efficiency",sep=""))$value))
  }
  #for(tech in c("oil","gas"))
  #  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter=paste(tech,"system_efficiency",sep="_"), value=boiler_efficiency_fun(sD,yeartime)  ))

  #scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="heat_pump_system_efficiency", value=heat_pump_cop_fun(sD,yeartime)  ))

  #system maintenance
  for(tech in technologies){
    scen <- dplyr::bind_rows(scen,tibble::tibble(parameter=paste(tech,"_system_maintenance",sep=""), value=  dplyr::filter(sD, parameter==paste(tech,"_system_maintenance",sep=""))$value))
  }

  #fuel prices
  for(fuel_type in c("oil","gas","electricity","solid_fuel")){
    scen <- dplyr::bind_rows(scen,tibble::tibble(parameter=paste(fuel_type,"price",sep="_"), value=energy_price_fun(fuel_type,sD,yeartime)  ))
  }
  #night rate vs day rate
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="night_rate_discount", value =  night_discount_fun(sD,yeartime)))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="night_rate_usage_factor",  value=dplyr::filter(sD, parameter=="night_rate_usage_factor")$value))
  #space heating
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="q_passive",  value=dplyr::filter(sD, parameter=="q_passive")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="q_hotwater",  value=dplyr::filter(sD, parameter=="q_hotwater")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="q_lighting",  value=dplyr::filter(sD, parameter=="q_lighting")$value))

  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="present_bias_threshold",  value=dplyr::filter(sD, parameter=="present_bias_threshold")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="rebound_threshold",  value=dplyr::filter(sD, parameter=="rebound_threshold")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="warmer_homes_target_ber",  value=dplyr::filter(sD, parameter=="warmer_homes_target_ber")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="warmer_homes_cost_cap",  value=dplyr::filter(sD, parameter=="warmer_homes_cost_cap")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="minimum_fabric_grant",  value=dplyr::filter(sD, parameter=="minimum_fabric_grant")$value))


  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="ber_upgrade_labour_cost_share",  value=dplyr::filter(sD, parameter=="ber_upgrade_labour_cost_share")$value))

  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="overhead",  value=dplyr::filter(sD, parameter=="overhead")$value))
  #scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="overhead",  value=dplyr::filter(sD, parameter=="vat_service")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="vat_service",  value=dplyr::filter(sD, parameter=="vat_service")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="vat_goods",  value=dplyr::filter(sD, parameter=="vat_goods")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="labour_cost", value =  labour_cost_fun(sD,yeartime)))
  #
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="pef_electricity", value =  pef_fun(sD,yeartime)))
  #hp grant dates
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="hp_grant_introduction",  value=dplyr::filter(sD, parameter=="hp_grant_introduction")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="hp_grant_increase",  value=dplyr::filter(sD, parameter=="hp_grant_increase")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="hp_grant_removal",  value=dplyr::filter(sD, parameter=="hp_grant_removal")$value))
  #ber upgrade dates
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="warmer_homes_introduction",  value=dplyr::filter(sD, parameter=="warmer_homes_introduction")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="better_energy_introduction",  value=dplyr::filter(sD, parameter=="better_energy_introduction")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="oss_introduction",  value=dplyr::filter(sD, parameter=="oss_introduction")$value))

  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="hli_heat_pump_threshold",  value=dplyr::filter(sD, parameter=="hli_heat_pump_threshold")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="warmer_homes_cost_scale",  value=dplyr::filter(sD, parameter=="warmer_homes_cost_scale")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="warmer_homes_heat_pump",  value=dplyr::filter(sD, parameter=="warmer_homes_heat_pump")$value))


  #scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="evening_tariff", value =  evening_tariff_fun(sD,yeartime)))
  #scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="night_tariff", value =  night_tariff_fun(sD,yeartime)))
  #scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="marginal_tax_rate", value =  dplyr::filter(sD, parameter=="marginal_tax_rate")$value))   scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="discount_rate", value =  dplyr::filter(sD, parameter=="discount_rate")$value))
  #ber upgrade cost model parameters
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="ber_upgrade_marginal_cost_k",  value=dplyr::filter(sD, parameter=="ber_upgrade_marginal_cost_k")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="ber_upgrade_marginal_cost_alpha",  value=dplyr::filter(sD, parameter=="ber_upgrade_marginal_cost_alpha")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="ber_upgrade_marginal_cost_c0",  value=dplyr::filter(sD, parameter=="ber_upgrade_marginal_cost_c0")$value))

  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="nu.", value =  dplyr::filter(sD, parameter=="nu.")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="lambda.", value =  dplyr::filter(sD, parameter=="lambda.")$value)) #probabaly set to zero
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="p.", value =  dplyr::filter(sD, parameter=="p.")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="r.", value =  dplyr::filter(sD, parameter=="r.")$value)) #delta = 1/(1+r)
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="rho.", value =  dplyr::filter(sD, parameter=="rho.")$value)) #rebound no rebound is rho=0
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="beta.", value =  dplyr::filter(sD, parameter=="beta.")$value)) #presenr bias
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="eta.", value =  dplyr::filter(sD, parameter=="eta.")$value)) #hassle hassle-free is eta=1
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="tau.", value =  dplyr::filter(sD, parameter=="tau.")$value)) #sludge - sludge-free is eta=1


  #return(scen)
  return(scen %>% fast_params())
}

#' fast_params
#'
#' helper function to convert a long format dataframe to an environment object, used for fast access to scenario parameters
#'
#' @param params_long long format dataframe with columns "parameter" and "value"
#'
#' @return environment object
#' @export
#'
#' @examples
fast_params <- function(params_long){

  test <- as.list(params_long$value)
  names(test) <- params_long$parameter
  test <- list2env(test)
  return(test)
}


#params <- scenario_params(sD,2026)


#' initialise_agents
#'
#' creates the agent initial state including model weights, stochastically imputed ground floor areas
#'
#'
#' @param sD scenario (usable_roof_fraction only)
#' @param yeartime start year (default 2010)
#' @param cal_run calibration run number between 1 and 100
#'
#' @return a dataframe with columns
#' @export
#'
#' @examples
#'
#' initialise_agents(sD,2015,10) #%>% system.time()
#'
initialise_agents <- function(sD,yeartime=2015,cal_run=10){

  #initialise to 2015
  params <- scenario_params(sD,yeartime)
  #only has dimensions 861??
  agents <- hp_model_weights_oo %>% dplyr::filter(calibration_run==cal_run) %>% dplyr::select(-calibration_run)
  #retain minimal set of features minimial set for imputing missing data
  hp_surv <- hp_survey_oo %>% dplyr::select(serial,qc2,q1,q2,q3,q5,q6,q11,qh,qb,qe,q4,qf,q7,q13,q121,q122,q123,q124,q125,q126,q127,q128,q129,actualage,qi)
  hp_surv <- recode_survey(hp_surv)
  #bias correct ber values
  #hp_surv <- hp_surve %>% dplyr::mutate()
  #select minimal featureset
  hp_surv <- hp_surv %>% dplyr::select(serial,qc2,q1,q2,construction_year,ber,hli,floor_area,primary_heat,heating_install_time,secondary_heat1,secondary_heat2,income,fuel_allowance)

  #if heating_install_year is later than yeartime look at earlier heating system installation, assuming the same technology was used,
  #unless it is a heat pump, in which case it is 50-50 oil gas at the moment.
  hp_surv <- hp_surv %>% dplyr::rowwise() %>% dplyr::mutate(heating_install_time = ifelse(heating_install_time <= yeartime, heating_install_time,
                      prior_install_year(heating_install_time,ifelse(primary_heat != "heat_pump",primary_heat,sample(c("oil","gas"),1)),initial_year=yeartime,params)))
  #ensure that prior install year cannot be older than construction year
  hp_surv <- hp_surv %>% dplyr::mutate(heating_install_time = pmax(construction_year,heating_install_time))
  #agents <- agents %>% dplyr::inner_join(pv_survey_oo %>% dplyr::select(ID,housecode,region,q1))
  agents <- agents %>% dplyr::inner_join(hp_surv,by="serial")  #some agents are missing!
  agents <- agents %>% dplyr::rename("region"=qc2,"house_type"=q1, "storeys"=q2)
  #remove houses built after 2015
  agents <- agents %>% dplyr::filter(construction_year < yeartime)
  agents <- agents %>% dplyr::mutate(kW=peak_heating_demand(hli,floor_area))
  agents$q52 <- 1 #assume that nobody knows a heat pump owner
  #agents$q52_up <- 1 #assume that nobody knows an upgrader
  agents$serial <- as.character(agents$serial)
  #HLI & BER
  #bias HLI upwards (upgrades & survey bias)
  agents <- agents %>% dplyr::mutate(hli = pmax(hli,0.7)) #don't allow hli values less than 0.7
  #re-compute BER
  agents <- agents %>% dplyr::rowwise() %>% dplyr::mutate(ber=ber_from_hli(hli,primary_heat,heating_install_time,params))
  #annualised heating cost
  agents <- agents %>% dplyr::rowwise() %>% dplyr::mutate(eac = annualised_heating_system_cost(hli,primary_heat,
                                      heating_install_time,"new",floor_area,house_type,construction_year,"None",params))
  agents <- agents %>% dplyr::rowwise() %>% dplyr::mutate(eac_actual = annualised_heating_system_cost(hli,primary_heat,
                                    heating_install_time,"new",floor_area,house_type,construction_year,"None",params,include_rebound = TRUE))

  #optionally remove some columns
  agents <- agents %>% dplyr::select(-secondary_heat1,-secondary_heat2)
  agents <- agents %>% dplyr::rename("tech"=primary_heat)
  return(agents %>% dplyr::ungroup())
}

# agents_in <- initialise_agents(sD,2015,100)
# social_network <- make_artificial_society(hp_society_oo %>% dplyr::filter(serial %in% agents_in$serial),homophily)
#

#' update_agents
#'
#' micro-simulation time-step updater
#'
#' The workhorse ABM function.Within a scenario, does a single month update of the agent characteristics. A random sample of agents evaluates their economic and social
#' utilities. If these exceed their individual threshold an optimal PV_BESS system is adopted.
#'
#'
#' @param sD  scenario dataframe
#' @param yeartime decimal time
#' @param agents_in input agent dataframe
#' @param social_network artifical social network
#' @param ignore_social option to ignore social effects. Default is FALSE.
#' @param cal_run microcalibration run index between 1 and 100
#' @param quiet TRUE to suppress messages
#'
#' @return updated agent dataframe
#' @export
#' @examples
#'
#' agents_in <- initialise_agents(sD,2015,100)
#' social_network <- make_artificial_society(hp_society_oo %>% dplyr::filter(serial %in% agents_in$serial),homophily)
#' agents_1 <- update_agents(sD,2026+1/6,agents_in,social_network,cal_run=10,quiet=FALSE)
#' agents_2 <- update_agents(sD,2026+1/3,agents_1,social_network,cal_run=10,quiet=FALSE)

update_agents <- function(sD,yeartime,agents_in, social_network,ignore_social=F,cal_run=50, quiet=TRUE){
  #
  cost_model <- "logistic"
  #beta. <- 0.2532785
  #params at yeartime
  params <- scenario_params(sD,yeartime)
  #
  tech_params <- tech_params_fun()
  #empirical partial utilites from microcalibration run cal_run
  empirical_u <- hp_empirical_utils %>% dplyr::filter(calibration_run==cal_run) %>% dplyr::select(-calibration_run)
  #social utility - knowing others who have installed an heat pump
  du_social <- dplyr::filter(empirical_u,question_code=="q52")$du_average
  theta <- dplyr::filter(empirical_u,question_code=="theta")$du_average

  a_s <- agents_in
  n_heat <- dim(a_s %>% dplyr::filter(tech=="heat_pump"))[1]
  a_s$upgrade <- FALSE
  a_s$failure <- FALSE
  a_s$upgrade_cost <- 0
  a_s$grant_type <- NA_character_
  a_s$tech_cost <- 0
  a_s$upgrade_grant <-0
  a_s$heat_pump_grant <- 0
  #calculate updated eac and eac_actual
  a_s <- a_s %>% dplyr::rowwise() %>% dplyr::mutate(eac = annualised_heating_system_cost(hli,tech,heating_install_time,"new",floor_area,house_type,construction_year,"None",params,include_rebound = FALSE))
  a_s <- a_s %>% dplyr::rowwise() %>% dplyr::mutate(eac_actual = annualised_heating_system_cost(hli,tech,heating_install_time,"new",floor_area,house_type,construction_year,"None",params,include_rebound = TRUE))
  #update definitions of old and new for all agents
  #a_s <- a_s %>% dplyr::mutate(S1_old=S1_new,S2_old = S2_new,B_old=B_new)
  #a_s <- a_s %>% dplyr::mutate(capex_old=capex_new,opex_old=opex_new)
  #this subsample of agents decide to look at rooftop pv
  #assing any heating tech breakdowns during timestep
  a_s <- a_s %>% dplyr::mutate(failure = weibull_failure(heating_install_time,params$yeartime,params$yeartime+1/6,tech))
  a_s <- dplyr::ungroup(a_s)
  #filter on failure
  b_s1 <- a_s %>% dplyr::filter(failure)
  #print(paste("number of heating system failures",dim(b_s1)[1]))
  b_s1 <- b_s1 %>% dplyr::select(-heat_pump_grant,-grant_type)
  #filter on system upgraders
  b_s2 <- dplyr::slice_sample(a_s,n=roundr(dim(a_s)[1]*params$p.))
  b_s2 <- b_s2 %>% dplyr::select(-heat_pump_grant)
  #print(paste("number of potential upgraders",dim(b_s2)[1]))
  #households that consider full upgrade following failure
  b_s3 <- b_s1 %>% dplyr::filter(serial %in% b_s2$serial)
  #exclude failure where upgrade is being implemented => just two categories failure and upgr
  b_s1 <- b_s1 %>% dplyr::filter(!(serial %in% b_s3$serial))

  hp_savings_env <- function(hli,tech, house_type,storeys, construction_year, region, floor_area) {
    heat_pump_savings(hli,tech, params$yeartime, house_type, storeys,construction_year, region, floor_area, params)
  }
  hp_upgrade_savings_env <- function(hli,tech, heating_install_time,house_type, storeys,construction_year, region, floor_area,fuel_allowance) {
    heat_pump_upgrade_savings(hli,tech, heating_install_time, house_type,storeys, construction_year, region, floor_area, cost_model,params,fuel_allowance,include_grants = TRUE)
  }

  ########################
  # Heating system failures
  ########################

  #logic: if has heat pump replace
  b_s0 <- b_s1 %>% dplyr::select(hli,tech,house_type, storeys,construction_year, region, floor_area)
  #df <- purrr::pmap(b_s0,optimise_heat_env)
  # should REBOUND be inlcuded at this step?
  df <- purrr::pmap(b_s0,hp_savings_env)
  df <- do.call(rbind,df)
  b_s1 <- b_s1 %>% dplyr::bind_cols(df)
    ##############################
    #heat_pump failures. choose between retaining the heat pump or switching to gas
    ##############################
  b_s1_hp <- b_s1 %>% dplyr::filter(tech=="heat_pump")
  #print(paste("number of heat pump failures", dim(b_s1_hp)[1]))
  # CORRECT
  if(dim(b_s1_hp)[1] != 0) {
    b_s1_hp_stick <- b_s1_hp %>% dplyr::filter(savings <= 0 & hli <= params$hli_heat_pump_threshold)
    b_s1_hp_switch <- b_s1_hp %>% dplyr::filter(savings > 0 | hli > params$hli_heat_pump_threshold )  #
    b_s1_hp_stick <- b_s1_hp_stick %>% dplyr::mutate(eac=eac_stick)
    #b_s1_hp_stick <- b_s1_hp_stick %>% dplyr::select(-eac_switch,-eac_stick,-savings,-hp_grant_type)
  #
    b_s1_hp_switch <- b_s1_hp_switch %>% dplyr::mutate(eac=eac_switch)
    #b_s1_hp_switch <- b_s1_hp_switch %>% dplyr::select(-eac_stick,-eac_switch,-savings,-hp_grant_type)
    b_s1_hp_switch$tech <- "gas"
    #print(paste("number of heat pump retainers", dim(b_s1_hp_stick)[1]))
    b_s1_hp <- b_s1_hp_stick %>% dplyr::bind_rows(b_s1_hp_switch) %>% dplyr::mutate(heating_install_time = params$yeartime)
  }
  b_s1_hp <- b_s1_hp %>% dplyr::select(-any_of("savings")) %>% dplyr::select(!dplyr::matches("switch|stick"))

    ################################
    #non heat pump failures
    ############################
  #reject heat pump adoption when financial utility does not overcome barriers
  b_s1_nhp <- b_s1 %>% dplyr::filter(tech != "heat_pump" )
  b_s1_nhp <- b_s1_nhp %>% dplyr::mutate(du_fin = -params$nu.*w_q13*savings)
  b_s1_nhp <- b_s1_nhp %>% dplyr::mutate(du_social = w_q52*du_social[q52])
  b_s1_nhp <- b_s1_nhp %>% dplyr::mutate(du_theta = w_theta*theta)
  #sum and include hypothetical bias correction (default is zero)
  b_s1_nhp <- b_s1_nhp %>% dplyr::mutate(du_tot = du_fin+du_social+du_theta + params$lambda.)
  #COULD ASSUME A HLI THRESHOLD FOR FAILURE ADOPTERS i.e. clearly heat pump ready
  b_s1_nhp_switch <- b_s1_nhp %>% dplyr::filter(du_tot > 0 & hli <= params$hli_heat_pump_threshold)
  b_s1_nhp_switch$tech <- "heat_pump"
  b_s1_nhp_stick <- b_s1_nhp %>% dplyr::filter(is.na(du_tot) | du_tot <= 0 | hli > params$hli_heat_pump_threshold)
  #print(paste("number of heat pump adopters",dim(b_s1_nhp_switch)[1]))
   #stickers
  b_s1_nhp_stick <- b_s1_nhp_stick %>% dplyr::mutate(eac=eac_stick)
  b_s1_nhp_stick <- b_s1_nhp_stick %>% dplyr::select(-du_fin,-du_social,-du_theta,-du_tot,-eac_stick,-eac_switch,-savings)
  b_s1_nhp_stick$heat_pump_grant <- 0
  #b_s1_stick$adopt <- FALSE
  #switchers
  b_s1_nhp_switch <- b_s1_nhp_switch %>% dplyr::mutate(eac=eac_switch)
  b_s1_nhp_switch <- b_s1_nhp_switch %>% dplyr::select(-du_fin,-du_social,-du_theta,-du_tot,-eac_stick,-eac_switch,-savings)
  b_s1_nhp <- b_s1_nhp_stick %>% dplyr::bind_rows(b_s1_nhp_switch) %>% dplyr::mutate(heating_install_time = params$yeartime)

  stopifnot(dim(b_s1_nhp_switch %>% dplyr::filter(hli > params$hli_heat_pump_threshold))[1] == 0)
  stopifnot(dim(b_s1_nhp)[1] + dim(b_s1_hp)[1] == dim(b_s1)[1])
  stopifnot(dim(b_s1_nhp_stick)[1] + dim(b_s1_nhp_switch)[1] == dim(b_s1_nhp)[1])
  if(dim(b_s1_hp)[1] != 0 ) stopifnot(dim(b_s1_hp_stick)[1] + dim(b_s1_hp_switch)[1] == dim(b_s1_hp)[1])
  b_s1 <- b_s1_hp %>% dplyr::bind_rows(b_s1_nhp)
  #calculate including rebound eac_actual
  b_s1 <- b_s1 %>% dplyr::select(!dplyr::matches("switch|stick"))

  ################################
  # Energy Efficiency upgrades
  #################################

  ######################
  # each agent chooses between (1) optimum fabric upgrade + sticking with current heating tech
  # (2) fabric upgrade and switching to heat pump
  # (3) reject upgrade do nothing
  ######################

  b_s2$upgrade <- TRUE
  b_s0 <- b_s2 %>% dplyr::select(hli,tech,heating_install_time,house_type,storeys, construction_year, region, floor_area,fuel_allowance)
  #df <- purrr::pmap(b_s0,optimise_heat_env)
  df <- purrr::pmap(b_s0,hp_upgrade_savings_env)
  df <- do.call(rbind,df)
  b_s2 <- b_s2 %>% dplyr::bind_cols(df)
  ####################################################################################
  # households that reject efficiency upgrade because of insufficient reward relative to disruption
  #####################################################################################
  print(paste(dim(b_s2 %>% dplyr::filter(eac_stick >= eac_old & eac_switch >= eac_old))[1],"upgrades rejected"))
  b_s2 <- b_s2 %>% dplyr::filter(eac_stick < eac_old | eac_switch < eac_old) %>% dplyr::select(-eac_old)
  #b_s2 %>% dplyr::select(tech,hli,hli_stick,hli_switch,eac_stick,eac_switch,savings)
    ################################################################
    # Efficiency upgrade where there is an existing heat pump
    #################################################################
  b_s2_hp <- b_s2 %>% dplyr::filter(tech=="heat_pump")
  #print(paste("b_s2_bp"))
  #print(b_s2_hp)
  if(nrow(b_s2_hp) == 0) b_s2_hp <- b_s2_hp %>% dplyr::select(-any_of("savings")) %>% dplyr::select(!dplyr::matches("switch|stick"))
  if(nrow(b_s2_hp) > 0){
    b_s2_hp_switch <- b_s2_hp %>% dplyr::filter(savings > 0)
    b_s2_hp_stick <- b_s2_hp %>% dplyr::filter(savings <= 0)
  #
    b_s2_hp_stick <- b_s2_hp_stick %>% dplyr::select(-savings)
    b_s2_hp_stick <- b_s2_hp_stick %>% dplyr::mutate(hli = hli_stick,eac=eac_stick,upgrade_cost=upgrade_cost_stick,
                                                          heating_sys_cost=heating_sys_cost_stick, grant_type="None")
    b_s2_hp_stick <- b_s2_hp_stick %>% dplyr::select(!dplyr::matches("switch|stick"))

    #b_s2_hp_switch <- b_s2_hp_switch %>% dplyr::select(-savings)
    b_s2_hp_switch <- b_s2_hp_switch %>% dplyr::mutate(hli = hli_switch,eac=eac_switch,upgrade_cost=upgrade_cost_switch,
                                                          heating_sys_cost=heating_sys_cost_switch, grant_type="None")
    b_s2_hp_switch <- b_s2_hp_switch %>% dplyr::select(!dplyr::matches("switch|stick"))

    b_s2_hp <- b_s2_hp_stick %>% dplyr::bind_rows(b_s2_hp_switch)
  }
      #####################################################################
    # Efficiency upgrade where existing heating tech is not a heat pump
    #####################################################################
    #Are financial savings are strong enough?
  b_s2_nhp <- b_s2 %>% dplyr::filter(tech!="heat_pump")
  #what happens if b_s2_nhp is empty?
  if(nrow(b_s2_nhp)==0)  b_s2_nhp <- b_s2_nhp %>% dplyr::select(-any_of("savings")) %>% dplyr::select(!dplyr::matches("switch|stick"))

  if(nrow(b_s2_nhp) > 0) {
   b_s2_nhp <- b_s2_nhp %>% dplyr::mutate(du_fin = -params$nu.*w_q13*savings) #financial
   b_s2_nhp <- b_s2_nhp %>% dplyr::mutate(du_social = w_q52*du_social[q52]) #social influence
   b_s2_nhp <- b_s2_nhp %>% dplyr::mutate(du_theta = w_theta*theta) #barrier
   #sum and include a possible hypothetical bias correction (default is zero but you might need it)
   b_s2_nhp <- b_s2_nhp %>% dplyr::mutate(du_tot = du_fin+du_social+du_theta + params$lambda.)
   #adopters
   b_s2_nhp_switch <- b_s2_nhp %>% dplyr::filter(du_tot > 0)
   #non-adopters
   b_s2_nhp_stick <- b_s2_nhp %>% dplyr::filter(is.na(du_tot) | du_tot <= 0)
   #clean up non-adopters e.g. remove redundant "switch" data
   b_s2_nhp_stick <- b_s2_nhp_stick %>% dplyr::select(-du_fin,-du_social,-du_theta,-du_tot,-savings)
   b_s2_nhp_stick <- b_s2_nhp_stick %>% dplyr::mutate(hli = hli_stick,eac = eac_stick,upgrade_cost=upgrade_cost_stick,
                                                     heating_sys_cost=heating_sys_cost_stick, grant_type=grant_type_stick,
                                                     upgrade_grant=upgrade_grant_stick,heat_pump_grant=heat_pump_grant_stick)
   b_s2_nhp_stick <- b_s2_nhp_stick %>% dplyr::select(!dplyr::matches("switch|stick"))
   #
   #clean up adopters e.g. remove redundant "stick" data
   b_s2_nhp_switch <- b_s2_nhp_switch %>% dplyr::select(-du_fin,-du_social,-du_theta,-du_tot,-savings)
   b_s2_nhp_switch <- b_s2_nhp_switch %>% dplyr::mutate(hli = hli_switch,eac = eac_switch,upgrade_cost=upgrade_cost_switch,
                                                     heating_sys_cost=heating_sys_cost_switch, grant_type=grant_type_switch,
                                                     upgrade_grant=upgrade_grant_switch,heat_pump_grant=heat_pump_grant_switch)
   b_s2_nhp_switch <- b_s2_nhp_switch %>% dplyr::select(!dplyr::matches("switch|stick"))
   b_s2_nhp_switch$tech <- "heat_pump"
   #combine adopters and non-adopters
   b_s2_nhp <- b_s2_nhp_stick %>% dplyr::bind_rows(b_s2_nhp_switch) %>% dplyr::mutate(heating_install_time = params$yeartime)
  }
  #combine all upgrades whether existing heat pump or not and update to the new kW installed capacities
  b_s2 <-  b_s2_hp %>% dplyr::bind_rows(b_s2_nhp) %>% dplyr::mutate(kW=heating_system_size(ber*floor_area))

  b_s2 <- b_s2 %>% dplyr::select(-any_of("savings")) %>% dplyr::select(!dplyr::matches("switch|stick"))

  stopifnot(dim(b_s2_nhp)[1] + dim(b_s2_hp)[1] == dim(b_s2)[1])
  if(dim(b_s2_nhp)[1] != 0) stopifnot(dim(b_s2_nhp_stick)[1]+dim(b_s2_nhp_switch)[1] == dim(b_s2_nhp)[1])
  if(dim(b_s2_hp)[1] != 0) stopifnot(dim(b_s2_hp_stick)[1]+dim(b_s2_hp_switch)[1] == dim(b_s2_hp)[1])

  #combine == dim(b_s2)[1])
  ##################################
  #combine failures and upgraders
  #################################
  b_s <- b_s1 %>% dplyr::bind_rows(b_s2)
  #compute new bers
  b_s <- b_s %>% dplyr::rowwise() %>% dplyr::mutate(ber=ber_from_hli(hli,tech,install_time = params$yeartime,params))

  #update agents
  a_s <- dplyr::filter(a_s, !(serial %in% b_s$serial))
  a_s <- dplyr::bind_rows(a_s,b_s) %>% dplyr::arrange(serial)
  a_s <- a_s %>% dplyr::mutate(adopt=(tech=="heat_pump"))
  #a_s <- a_s %>% dplyr::mutate(kW=heating_system_size(ber*floor_area))
  #recompute social variable
  ma <- igraph::as_adjacency_matrix(social_network)
  g <- social_network %>% tidygraph::activate(nodes) %>% dplyr::left_join(a_s,by="serial")
  #social network conformity effect
  adopter_nodes <- igraph::V(g)$adopt==TRUE
  a_s$q52 <- as.numeric(ma %*% adopter_nodes) #social reinforcement 0 no adoption 1 adoption
  if(ignore_social) a_s$q52 <- 1 #no adopters assumed present in local network
  a_s <- a_s %>% dplyr::rowwise() %>% dplyr::mutate(q52 = min(q52+1,4)) #update q52 encoding 1,2,3,4
  #agents_out <- a_s
  #a_s <- a_s %>% dplyr::select(-du_tot)
  if(!quiet) {
    print(paste("time", round(yeartime,1), "number of system breakdowns",dim(b_s1)[1]))
    print(paste("time", round(yeartime,1), "number of efficiency upgrades",dim(b_s2)[1]))
    print(paste("time", round(yeartime,1), "number of heat pump adopters following system breakdown",dim(b_s1_nhp %>% dplyr::filter(tech=="heat_pump"))[1]))
    print(paste("number of heat pump adopters as part of efficiency upgrade",dim(b_s2_nhp %>% dplyr::filter(tech=="heat_pump"))[1]))
  }
  a_s <- a_s %>% dplyr::select(-adopt)
  return(dplyr::ungroup(a_s))
}


#' runABM
#'
#' Runs home energy efficiency system adoption simulation on artificial society of ~792 agents.
#' Each run is performed on an independently generated social network with randomisation from initialise_agents() and
#' with a random micro_calibration run (index 1..100)
#'
#' Bi-monthly timesteps.
#'
#' Good luck.
#'
#' @param sD scenario set-up dataframe, typically read with readr::read_xlxs(...,sheet=scenario)
#' @param Nrun integer, number runs
#' @param simulation_end the final year of simulation of early termination is required
#' @param resample_society if TRUE resample hp_society_oo with replacement to capture additional variability
#' @param n_unused_cores number of cores left unused in parallel/foreach. Recommended values 2 or 1.
#' @param use_parallel if TRUE uses multiple cores. Use FALSE for diagnostic runs on a single core.
#' @param ignore_social if TRUE ignore social network effects. Default is FALSE
#' @param quiet if TRUE messaging is reduced
#'
#' @return a three component list - simulation output, scenario setup, meta-parameters
#' @export
#' @importFrom magrittr %>%
#' @importFrom lubridate %m+%
#' @importFrom foreach %dopar%
#'
runABM <- function(sD, Nrun=1,simulation_end=2030,resample_society=F,n_unused_cores=2, use_parallel=T,ignore_social=F, quiet=TRUE){
  #
  year_zero <- 2015
  #calibration params:: MOVED TO SYSTDATA WHEN CALIBRATION COMPLETE
  p <- sD %>% dplyr::filter(parameter=="p.") %>% dplyr::pull(value)
  nu <- sD %>% dplyr::filter(parameter=="nu.") %>% dplyr::pull(value)
  #lambda <- sD %>% dplyr::filter(parameter=="lambda.") %>% dplyr::pull(value)
  beta <- sD %>% dplyr::filter(parameter=="beta.") %>% dplyr::pull(value)
  eta <- sD %>% dplyr::filter(parameter=="eta.") %>% dplyr::pull(value)
  rho <- sD %>% dplyr::filter(parameter=="rho.") %>% dplyr::pull(value)
  r <- sD %>% dplyr::filter(parameter=="r.") %>% dplyr::pull(value)
  tau <-  sD %>% dplyr::filter(parameter=="tau.") %>% dplyr::pull(value)
  #
  print(paste("financial utility scale (nu.)=",round(nu,2),"p.=",round(p,4),"discount_rate (r)=",round(r,2),"rebound=",round(rho,2), "present bias=",round(beta,2),"disruption=",round(eta,3),"sludge=",round(tau,3)))
  #seai_elec <- pvbessmicrosimr::seai_elec
  #bi-monthly runs
  Nt <- round((simulation_end-year_zero+1)*6)
  #annual runs
  #Nt <- round((simulation_end-year_zero+1))
  #agents0 <- agents_i
  #cal_run <- 40
  #u_empirical <- empirical_utils_oo %>% dplyr::filter(calibration==cal_run) %>% dplyr::select(-calibration)
  #
  if(use_parallel){
    #
    number_of_cores <- parallel::detectCores() - n_unused_cores
    doParallel::registerDoParallel(number_of_cores)

    abm <- foreach::foreach(j = 1:Nrun, .packages = "dplyr", .combine=dplyr::bind_rows,.export = c("initialise_agents","update_agents","make_artificial_society")) %dopar% {
      #abm <- foreach::foreach(j = 1:Nrun, .errorhandling = "pass",.export = c("initialise_agents","update_agents4")) %dopar% {

            #randomiise ICEV emissions assignment
      #choose segments
    microcal_run <- sample(1:100,1)
    agents_in <- initialise_agents(sD,year_zero,microcal_run)
    u_empirical <- hp_empirical_utils %>% dplyr::filter(calibration_run==microcal_run) %>% dplyr::select(-calibration_run)

      #create a new artificial society for each run
      print(paste("Generating network for run",j,"...."))
      if(!resample_society) social <- make_artificial_society(hpmicrosimr::hp_society_oo %>% dplyr::filter(serial %in% agents_in$serial),hpmicrosimr::homophily,4.5)
      if(resample_society){
        agent_resample <- sample(1:dim(hpmicrosimr::hp_society_oo)[1],replace=T)
        society_new <- society[agent_resample,]
        society_new$ID <- 1:dim(hpmicrosimr::hp_society_oo)[1]
        social <- make_artificial_society(society_new,hpmicrosimr::homophily,4.5)
      }

      #no transactions
      #agents_in$transaction <- FALSE
      agent_ts <- vector("list",Nt)
      agent_ts[[1]] <- agents_in #agent parameters with regularized weights

      for(t in seq(2,Nt)){
        #bi-monthly
        yeartime <- year_zero+(t-1)/6
        agent_ts[[t]] <- update_agents(sD,yeartime,agent_ts[[t-1]],social_network=social,ignore_social,cal_run=microcal_run,quiet) #static social network, everything else static
      }

      for(t in 1:Nt) agent_ts[[t]]$t <- t
      agent_ts <- tibble::as_tibble(data.table::rbindlist(agent_ts,fill=T))
      agent_ts$simulation <- j
      #add vertex degree
      degrees <- tibble::tibble(serial=social %>% tibble::as_tibble() %>% dplyr::pull(serial),degree=igraph::degree(social))
      agent_ts <- agent_ts %>% dplyr::inner_join(degrees)
      agent_ts
    }

    meta <- tibble::tibble(parameter=c("Nrun","end_year","p.","nu.","rho.","r.","beta.","eta.","tau."),value=c(Nrun,simulation_end,p,nu,rho,r,beta,eta,tau))
    abm <- abm %>% dplyr::mutate(date=lubridate::ymd(paste(year_zero,"-01-01",sep="")) %m+% months((t-1)*2)) %>% dplyr::arrange(simulation,date) %>% dplyr::select(-t)
    return(list("abm"=abm,"scenario"=sD,"system"=meta))
  }

  #don't use parallel
  #comment in next two lines for parallel
  if(!use_parallel){

    abm <- tibble::tibble()
    #number_of_cores <- parallel::detectCores() - n_unused_cores
    #doParallel::registerDoParallel(number_of_cores)
    #comment out next line for parallel
    for(j in 1:Nrun){
      #comment in next line for parallel
      #abm <- foreach::foreach(j = 1:Nrun, .combine=dplyr::bind_rows,.export = c("initialise_segments","update_agents4")) %dopar% {
      #create a new artificial society for each run
      #randomise ICEV emissions assignment
      #choose market segment for each agent
      microcal_run <- sample(1:100,1)
      u_empirical <- hp_empirical_utils %>% dplyr::filter(calibration_run==microcal_run) %>% dplyr::select(-calibration_run)
      agents_in <- initialise_agents(sD,year_zero,microcal_run)
      #
      print(paste("Generating network for run",j,"...."))
      #u_empirical <- empirical_utils_oo %>% dplyr::select(calibration=)
      if(!resample_society) social <- make_artificial_society(hp_society_oo  %>% dplyr::filter(serial %in% agents_in$serial),homophily,4.5)

      if(resample_society){
        agent_resample <- sample(1:dim(pv_society_oo)[1],replace=T)
        society_new <- pv_society_oo[agent_resample,]
        society_new$ID <- 1:dim(pv_society_oo)[1]
        social <- make_artificial_society(society_new,homophily,5)

      }
      #no transactions
      #agents_in$transaction <- FALSE
      agent_ts <- vector("list",Nt)
      agent_ts[[1]] <- agents_in #agent parameters with regularized weights

      for(t in seq(2,Nt)){
        #
        #yeartime <- year_zero+(t-1)
        yeartime <- year_zero+(t-1)/6
        agent_ts[[t]] <- update_agents(sD,yeartime,agent_ts[[t-1]],social_network=social,ignore_social,cal_run=microcal_run,quiet) #static social network, everything else static
        #agent_ts[[t]] <- tibble::tibble(t=t)
      }

      for(t in 1:Nt) agent_ts[[t]]$t <- t
      agent_ts <- tibble::as_tibble(data.table::rbindlist(agent_ts,fill=T))
      agent_ts$simulation <- j
      #network degree
      degrees <- tibble::tibble(serial=social %>% tibble::as_tibble() %>% dplyr::pull(serial),degree=igraph::degree(social))
      agent_ts <- agent_ts %>% dplyr::inner_join(degrees)
      abm <- dplyr::bind_rows(abm,agent_ts)
      #comment in next line for parallel
      #agent_ts
    }

    #meta <- tibble::tibble(parameter=c("Nrun","end_year","beta.","lambda.","p."),value=c(Nrun,simulation_end,beta,lambda,p))
    meta <- tibble::tibble(parameter=c("Nrun","end_year","beta.","p.","nu.","rho.","r.","eta.","tau."),value=c(Nrun,simulation_end,beta,p,nu,rho,r,eta,tau))
    #replace "t" with dates
    abm <- abm %>% dplyr::mutate(date=lubridate::ymd(paste(year_zero,"-01-01",sep="")) %m+% months((t-1)*2)) %>% dplyr::arrange(simulation,date) %>% dplyr::select(-t)
    closeAllConnections()
    return(list("abm"=abm,"scenario"=sD,"system"=meta))
  }

}

#' @title calABM
#'
#' @description
#' macro-calibration of hpmicrosimr. This is the Windows version.\cr
#' \cr
#' based on heat pump uptake, grant uptake and number of b2s on 1 nov 2025\cr
#' \cr
#' The output is summarised using summarise_cal()
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
#' @param rho rebound


#' @returns calibration run data
#' @export
#'
#' @examples
#' #test <- calABM(sD,4,2,TRUE,nu=0.4,p=0.006,beta = 0.8,r = 0.03,eta = 0.02,tau = 0.02,rho=0.3)
#'
calABM <- function(sD, Nrun=4,n_unused_cores=2, use_parallel=T, nu=0.27,p=0.0022,r=0.04,beta=0.8,eta=0.02,tau=0.02,rho=0.3){
  #
  year_zero <- 2015
  simulation_end <- 2025
  resample_society=F
  ignore_social=F
  #the 6-7 calibration parameters
  sD_cal <- sD
  sD_cal[sD_cal$parameter=="nu.","value"] <- nu #financial partial utility scale
  sD_cal[sD_cal$parameter=="p.","value"] <- p #additional hypothetical bias correction
  sD_cal[sD_cal$parameter=="beta.","value"] <- beta #present bias
  sD_cal[sD_cal$parameter=="eta.","value"] <- eta #hassle/sludge
  sD_cal[sD_cal$parameter=="rho.","value"] <- rho #rebound effect
  sD_cal[sD_cal$parameter=="r.","value"] <- r #risk-free or bare discount rate
  sD_cal[sD_cal$parameter=="tau.","value"] <- tau  #sludge

  lambda <- 0
  #calibration params:: MOVED TO SYSTDATA WHEN CALIBRATION COMPLETE
  print(paste("nu.=",nu,"p.=",p,"beta.=",beta,"r.=",r, "eta.=",eta,"tau.=",tau,"rho.=",rho))
  #bi-monthly runs
  Nt <- round((simulation_end-year_zero+1)*6)

  if(use_parallel){

    number_of_cores <- parallel::detectCores() - n_unused_cores
    cl <- parallel::makeCluster(number_of_cores)
    doParallel::registerDoParallel(cl)

    #abm <- foreach::foreach(j = 1:Nrun, .packages = "dplyr", .final = function(x) { parallel::stopCluster(cl); x },
    abm <- foreach::foreach(j = 1:Nrun, .packages = "dplyr",
                            .combine=dplyr::bind_rows,.export = c("initialise_agents","update_agents","make_artificial_society")) %dopar% {
      #abm <- foreach::foreach(j = 1:Nrun, .errorhandling = "pass",.export = c("initialise_agents","update_agents4")) %dopar% {

      #create a new artificial society for each run
      #print(paste("Generating network for run",j,"...."))
      microcal_run <- sample(1:100,1)
      u_empirical <- hpmicrosimr::hp_empirical_utils %>% dplyr::filter(calibration_run==microcal_run) %>% dplyr::select(-calibration_run)
      agents_in <- initialise_agents(sD_cal,year_zero,microcal_run)

      if(!resample_society) social <- make_artificial_society(hp_society_oo  %>% dplyr::filter(serial %in% agents_in$serial),homophily,4.5)

      if(resample_society){
        agent_resample <- sample(1:dim(hp_society_oo)[1],replace=T)
        society_new <- society[agent_resample,]
        society_new$ID <- 1:dim(hp_society_oo)[1]
        social <- make_artificial_society(society_new,hpmicrosimr::homophily,4.5)
      }
      #randomiise ICEV emissions assignment
      #choose segments
      #no transactions
      #agents_in$transaction <- FALSE
      agent_ts<- vector("list",Nt)
      agent_ts[[1]] <- agents_in #agent parameters with regularized weights

      for(t in seq(2,Nt)){
        #bi-monthly
        yeartime <- year_zero+(t-1)/6
        agent_ts[[t]] <- update_agents(sD_cal,yeartime,agent_ts[[t-1]],social_network=social,ignore_social,cal_run=microcal_run,quiet=TRUE) #static social network, everything else static
        #agent_ts[[t]] <- tibble::tibble(t=t)
      }

      for(t in 1:Nt) agent_ts[[t]]$t <- t
      agent_ts <- tibble::as_tibble(data.table::rbindlist(agent_ts,fill=T))
      agent_ts$simulation <- j
      #add vertex degree
      degrees <- tibble::tibble(serial=social %>% tibble::as_tibble() %>% dplyr::pull(serial),degree=igraph::degree(social))
      agent_ts <- agent_ts %>% dplyr::inner_join(degrees)
      agent_ts
    }
    parallel::stopCluster(cl)
    #closeAllConnections()

    meta <- tibble::tibble(parameter=c("Nrun","end_year","nu.","p.","r.","beta.","eta.","tau.","rho."),value=c(Nrun,simulation_end,nu,p,r,beta,eta,tau,rho))
    abm <- abm %>% dplyr::mutate(date=lubridate::ymd(paste(year_zero,"-01-01",sep="")) %m+% months((t-1)*2)) %>% dplyr::arrange(simulation,date) %>% dplyr::select(-t)
    cal_date <- "2025-11-01"
    start_date <- "2015-01-01"
    cal_dates <- c(start_date,"2021-01-01",cal_date) #two dates for calibration
    test <- summarise_abm_cal(abm,cal_date)
    heat_pumps <- test$tech %>% dplyr::filter(date %in% cal_dates,tech == "heat_pump") %>% dplyr::ungroup() %>% dplyr::select(date,n_tech)
    heat_pumps <- heat_pumps %>% dplyr::rename("n_heat_pump"=n_tech)
    efficiencies <- test$efficiency %>% dplyr::filter(date %in% cal_dates) %>% dplyr::ungroup()
    efficiencies <- efficiencies %>% dplyr::inner_join(heat_pumps)
    grants <- test$grants %>% dplyr::filter(date %in% cal_dates)
    print(paste("evaluating summary"))
    #print(test)
    n_heat_pump <- test$tech %>% dplyr::filter(date==lubridate::ymd(cal_date),tech=="heat_pump") %>% dplyr::pull(n_tech)
    n_heat_pump_0 <- test$tech %>% dplyr::filter(date==lubridate::ymd(start_date),tech=="heat_pump") %>% dplyr::pull(n_tech)
    #
    n_oss <- test$grants %>% dplyr::filter(date==lubridate::ymd(cal_date), grant_type=="OSS") %>% dplyr::pull(n_grant)
    n_betterenergy <- test$grants %>% dplyr::filter(date==lubridate::ymd(cal_date), grant_type=="BetterEnergyHomes") %>% dplyr::pull(n_grant)
    n_warmerhomes <- test$grants %>% dplyr::filter(date==lubridate::ymd(cal_date), grant_type=="WarmerHomes") %>% dplyr::pull(n_grant)
    n_grant <- n_oss + n_warmerhomes+n_betterenergy
    #print(n_grant)
    #
    cost_oss <- test$grants %>% dplyr::filter(date==lubridate::ymd(cal_date), grant_type=="OSS") %>% dplyr::pull(grants_Meuro)
    cost_betterenergy <- test$grants %>% dplyr::filter(date==lubridate::ymd(cal_date), grant_type=="BetterEnergyHomes") %>% dplyr::pull(grants_Meuro)
    cost_warmerhomes <- test$grants %>% dplyr::filter(date==lubridate::ymd(cal_date), grant_type=="WarmerHomes") %>% dplyr::pull(grants_Meuro)
    #
    n_b2 <- test$efficiency %>% dplyr::filter(date==lubridate::ymd(cal_date)) %>% dplyr::pull(n_b2)
    n_b2_0 <- test$efficiency %>% dplyr::filter(date==lubridate::ymd(start_date)) %>% dplyr::pull(n_b2)
    n_upgrade <- test$efficiency %>% dplyr::filter(date==lubridate::ymd(cal_date)) %>% dplyr::pull(n_b2)
    #print(n_b2_0)
    cals <- tibble::tibble(beta.=beta,eta.=eta,p.=p,nu.=nu,rho.=rho,r.=r)
    #print(cals)
    cals <- cals %>% dplyr::bind_cols(tibble::tibble(n_heat=n_heat_pump-n_heat_pump_0,n_heat_2015=n_heat_pump_0,number_b2=n_b2-n_b2_0,number_b2_2015=n_b2_0,
                                             oss_total=cost_oss,warmerhomes_total=cost_warmerhomes,betterenergy_total=cost_betterenergy,
                                              n_oss = n_oss,n_warmerhomes=n_warmerhomes,n_betterenergy=n_betterenergy,
                                             n_fabric_total=n_upgrade))
    #print(cals)
    #print(cals %>% dplyr::bind_cols(tibble::tibble(betterenergy_cost=cost_betterenergy)))
    #print(cals)
    #closeAllConnections()
    #return(cals)
  }
  print("exited loop")
  closeAllConnections()
  if(use_parallel) return(list(parameters=cals,efficiency=efficiencies,grants=grants))
  #print("DEBUG: We left the parallel block without returning!")
  #don't use parallel
  #comment in next two lines for parallel
  if(!use_parallel){

    abm <- tibble::tibble()
    #number_of_cores <- parallel::detectCores() - n_unused_cores
    #doParallel::registerDoParallel(number_of_cores)
    #comment out next line for parallel
    for(j in 1:Nrun){
      #comment in next line for parallel
      #abm <- foreach::foreach(j = 1:Nrun, .combine=dplyr::bind_rows,.export = c("initialise_segments","update_agents4")) %dopar% {
      #create a new artificial society for each run
      microcal_run <- sample(1:100,1)
      u_empirical <- hp_empirical_utils %>% dplyr::filter(calibration_run==microcal_run) %>% dplyr::select(-calibration_run)
      agents_in <- initialise_agents(sD_cal,year_zero,microcal_run)

      print(paste("Generating network for run",j,"...."))
      #u_empirical <- empirical_utils_oo %>% dplyr::select(calibration=)
      if(!resample_society) social <- make_artificial_society(hp_society_oo  %>% dplyr::filter(serial %in% agents_in$serial),homophily,4.5)
      if(resample_society){
        agent_resample <- sample(1:dim(hp_society_oo %>% dplyr::filter(serial %in% agents_in$serial))[1],replace=T)
        society_new <- hp_society_oo[agent_resample,]
        #society_new$ID <- 1:dim(pv_society_oo)[1]
        social <- make_artificial_society(society_new,homophily,5)

      }
        #no transactions
      #agents_in$transaction <- FALSE
      agent_ts <- vector("list",Nt)
      agent_ts[[1]] <- agents_in #agent parameters with regularized weights

      for(t in seq(2,Nt)){
        #
        #yeartime <- year_zero+(t-1)
        yeartime <- year_zero+(t-1)/6
        agent_ts[[t]] <- update_agents(sD_cal,yeartime,agent_ts[[t-1]],social_network=social,ignore_social,cal_run=microcal_run,quiet=FALSE) #static social network, everything else static
        #agent_ts[[t]] <- tibble::tibble(t=t)
      }

      for(t in 1:Nt) agent_ts[[t]]$t <- t
      agent_ts <- tibble::as_tibble(data.table::rbindlist(agent_ts,fill=T))
      agent_ts$simulation <- j
      #network degree
      degrees <- tibble::tibble(serial=social %>% tibble::as_tibble() %>% dplyr::pull(serial),degree=igraph::degree(social))
      agent_ts <- agent_ts %>% dplyr::inner_join(degrees)
      abm <- dplyr::bind_rows(abm,agent_ts)
      #comment in next line for parallel
      #agent_ts
    }
    meta <- tibble::tibble(parameter=c("Nrun","end_year","nu.","p.","r.","beta.","eta.","rho."),value=c(Nrun,simulation_end,nu,p,r,beta,eta,rho))
    #replace "t" with dates
    abm <- abm %>% dplyr::mutate(date=lubridate::ymd(paste(year_zero,"-03-01",sep="")) %m+% months((t-1)*2)) %>% dplyr::arrange(simulation,date) %>% dplyr::select(-t)
    #
    #print(abm)
    housing_stock_oo <- 611877+535675 #2016 census
    cal0 <- abm %>% dplyr::filter(date==lubridate::ymd("2025/11/01")) %>% dplyr::group_by(simulation) %>% dplyr::summarise(n0=dplyr::n())
    cal <-  abm %>% dplyr::filter(date <= lubridate::ymd("2025/11/01")) %>% dplyr::group_by(simulation,date,tech) %>% dplyr::summarise(n=dplyr::n())
    cal <- cal %>% dplyr::inner_join(cal0) %>% dplyr::mutate(n=n/n0*housing_stock_oo) %>% dplyr::select(-n0)
    cal <- cal %>% dplyr::group_by(tech,date) %>% dplyr::summarise(n=mean(n))
    #
    cal1 <- abm %>% dplyr::filter(date <= lubridate::ymd("2025/11/01")) %>% dplyr::group_by(simulation,date) %>% dplyr::summarise(n_b2=sum(ber < 125))
    cal1 <- cal1 %>% dplyr::inner_join(cal0) %>% dplyr::mutate(n_b2=n_b2/n0*housing_stock_oo) %>% dplyr::select(-n0)
    cal1 <- cal1 %>% dplyr::group_by(date) %>% dplyr::summarise(n_b2=mean(n_b2))
    #
    #cal <-  abm %>% dplyr::filter(date <= lubridate::ymd("2025/11/01"),tech=="heat_pump", heat_pump_grant > 0) %>% dplyr::group_by(simulation,date) %>% dplyr::summarise(n=dplyr::n())
    #cal <- cal %>% dplyr::inner_join(cal0) %>% dplyr::mutate(n=n/n0*housing_stock_oo) %>% dplyr::select(-n0)
    #print("Cals1")
    n_heat_pump <- cal %>% dplyr::filter(date==lubridate::ymd("2025/11/01"), tech=="heat_pump") %>% dplyr::pull(n) - cal %>% dplyr::filter(date==lubridate::ymd("2015/03/01"), tech=="heat_pump") %>% dplyr::pull(n)
    #grant assisted heat pumps
    n_b2 <- cal1 %>% dplyr::filter(date==lubridate::ymd("2025/11/01")) %>% dplyr::pull(n_b2) - cal1 %>% dplyr::filter(date==lubridate::ymd("2015/03/01")) %>% dplyr::pull(n_b2)
    #grants
    cal2 <-  abm %>% dplyr::filter(date <= lubridate::ymd("2025/11/01"), !is.na(grant_type), grant_type != "None") %>% dplyr::group_by(simulation,grant_type) %>% dplyr::summarise(grant=sum(heat_pump_grant+upgrade_grant))
    cal2 <- cal2 %>% dplyr::inner_join(cal0) %>% dplyr::mutate(grant=grant/n0*housing_stock_oo) %>% dplyr::select(-n0)
    cal2 <- cal2 %>% dplyr::group_by(grant_type) %>% dplyr::summarise(grant=mean(grant))
    cost_warmerhomes <- cal2 %>% dplyr::filter(grant_type=="WarmerHomes") %>% dplyr::pull(grant)/1e+6
    cost_oss <- cal2 %>% dplyr::filter(grant_type=="OSS") %>% dplyr::pull(grant)/1e+6
    cost_betterenergy <- cal2 %>% dplyr::filter(grant_type=="BetterEnergyHomes") %>% dplyr::pull(grant)/1e+6
    #print(paste("cost BetterEnergy",cost_betterenergy))
    #print(paste("cost WarmerHomes",cost_warmerhomes))
    #print(paste("cost OSS",cost_oss))
    #print(paste("n_heat=",n_heat_pump))
    #print(paste("number_b2=",n_b2))
    #cals <- tibble::tibble(beta.=beta,eta.=eta,p.=p,nu.=nu,rho.=rho,r.=r, n_heat=n_heat_pump,number_b2=n_b2, oss_cost=cost_oss,betterenergy_cost=cost_betterenergy,
    #               warmerhomes_cost=cost_warmerhomes)
    #print(cals)
    cals <- tibble::tibble(beta.=beta,eta.=eta,p.=p,nu.=nu,rho.=rho,r.=r,n_heat=n_heat_pump,number_b2=n_b2,oss_total=cost_oss,warmerhomes_total=cost_warmerhomes)#,betterenergy_total=cost_betterenergy)
    #print(cals)
    #print(cals %>% dplyr::bind_cols(tibble::tibble(betterenergy_cost=cost_betterenergy)))
    closeAllConnections()
    cals %>% return()

  }
  if(!use_parallel) return(cals)
}

#' calABM2
#'
#' macro-calibration of hpmicrosimr using mclapply on linux systems\cr
#'\cr
#' returns the same calibration data as calABM
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

#'
#' @returns
#' @export
#'
#' @examples
#' #test <- calABM2(sD,2,2,F)

calABM2 <- function(sD, Nrun=4,n_unused_cores=2, use_parallel=T, nu=0.4,p=0.004,beta=0.8,r=0.03,eta=0.03,tau=0.02,rho=0.3){
  #
  year_zero <- 2015
  simulation_end <- 2025
  resample_society=F
  ignore_social=F
  #the 6-7 calibration parameters
  sD_cal <- sD
  sD_cal[sD_cal$parameter=="nu.","value"] <- nu #financial partial utility scale
  sD_cal[sD_cal$parameter=="p.","value"] <- p #additional hypothetical bias correction
  sD_cal[sD_cal$parameter=="beta.","value"] <- beta #present bias
  sD_cal[sD_cal$parameter=="eta.","value"] <- eta #hassle/sludge
  sD_cal[sD_cal$parameter=="rho.","value"] <- rho #rebound effect
  sD_cal[sD_cal$parameter=="r.","value"] <- r #risk-free or bare discount rate
  sD_cal[sD_cal$parameter=="tau.","value"] <- tau  #sludge

  lambda <- 0
  #calibration params:: MOVED TO SYSTDATA WHEN CALIBRATION COMPLETE
  print(paste("nu.=",nu,"p.=",p,"beta.=",beta,"r.=",r, "eta.=",eta,"tau.=",tau,"rho.=",rho))
  #bi-monthly runs
  Nt <- round((simulation_end-year_zero+1)*6)


  # Define the function to run in parallel
  run_simulation <- function(j) {
    # Create a new artificial society for each run
    print(paste("Generating network for run", j, "...."))
    microcal_run <- sample(1:100,1)
    u_empirical <- hpmicrosimr::hp_empirical_utils %>% dplyr::filter(calibration_run==microcal_run) %>% dplyr::select(-calibration_run)
    agents_in <- initialise_agents(sD_cal,year_zero,microcal_run)




    if (!resample_society) {
      #social <- make_artificial_society(hpmicrosimr::hp_society_oo, hpmicrosimr::homophily, 5)
      social <- make_artificial_society(hpmicrosimr::hp_society_oo %>% dplyr::filter(serial %in% agents_in$serial),hpmicrosimr::homophily,4.5)

    } else {
      agent_resample <- sample(1:dim(hpmicrosimr::hp_society_oo)[1], replace = TRUE)
      society_new <- society[agent_resample, ]
      society_new$ID <- 1:dim(hpmicrosimr::hp_society_oo)[1]
      social <- make_artificial_society(society_new, hpmicrosimr::homophily, 4.5)
    }

    # Randomize ICEV emissions assignment
    agent_ts <- vector("list", Nt)
    agent_ts[[1]] <- agents_in  # Agent parameters with regularized weights

    for (t in seq(2, Nt)) {
      # Bi-monthly
      yeartime <- year_zero + (t - 1) / 6
      agent_ts[[t]] <- update_agents(sD_cal, yeartime, agent_ts[[t - 1]], social_network = social, ignore_social, cal_run = microcal_run)
    }

    for (t in 1:Nt) agent_ts[[t]]$t <- t
    agent_ts <- tibble::as_tibble(data.table::rbindlist(agent_ts, fill = TRUE))
    agent_ts$simulation <- j

    # Add vertex degree
    degrees <- tibble::tibble(serial=social %>% tibble::as_tibble() %>% dplyr::pull(serial),degree=igraph::degree(social))
    agent_ts <- agent_ts %>% dplyr::inner_join(degrees)

    return(agent_ts)
  }

  # Main parallel execution
  number_of_cores <- parallel::detectCores() - n_unused_cores

  # Run simulations in parallel using mclapply
   if(use_parallel) abm_list <- parallel::mclapply(1:Nrun, run_simulation, mc.cores = number_of_cores)
   if(!use_parallel) abm_list <- lapply(1:Nrun, run_simulation)

  #closeAllConnections()
  # Combine results into a single tibble
  abm <- dplyr::bind_rows(abm_list)

  meta <- tibble::tibble(parameter=c("Nrun","end_year","nu.","p.","r.","beta.","eta.","tau.","rho."),value=c(Nrun,simulation_end,nu,p,r,beta,eta,tau,rho))
  abm <- abm %>% dplyr::mutate(date=lubridate::ymd(paste(year_zero,"-01-01",sep="")) %m+% months((t-1)*2)) %>% dplyr::arrange(simulation,date) %>% dplyr::select(-t)
  cal_date <- "2025-11-01"
  start_date <- "2015-01-01"
  cal_dates <- c(start_date,"2021-01-01",cal_date) #two dates for calibration
  test <- summarise_abm_cal(abm,cal_date)
  heat_pumps <- test$tech %>% dplyr::filter(date %in% cal_dates,tech == "heat_pump") %>% dplyr::ungroup() %>% dplyr::select(date,n_tech)
  heat_pumps <- heat_pumps %>% dplyr::rename("n_heat_pump"=n_tech)
  efficiencies <- test$efficiency %>% dplyr::filter(date %in% cal_dates) %>% dplyr::ungroup()
  efficiencies <- efficiencies %>% dplyr::inner_join(heat_pumps)
  grants <- test$grants %>% dplyr::filter(date %in% cal_dates)
  print(paste("evaluating summary"))
  #print(test)
  n_heat_pump <- test$tech %>% dplyr::filter(date==lubridate::ymd(cal_date),tech=="heat_pump") %>% dplyr::pull(n_tech)
  n_heat_pump_0 <- test$tech %>% dplyr::filter(date==lubridate::ymd(start_date),tech=="heat_pump") %>% dplyr::pull(n_tech)
  #
  n_oss <- test$grants %>% dplyr::filter(date==lubridate::ymd(cal_date), grant_type=="OSS") %>% dplyr::pull(n_grant)
  n_betterenergy <- test$grants %>% dplyr::filter(date==lubridate::ymd(cal_date), grant_type=="BetterEnergyHomes") %>% dplyr::pull(n_grant)
  n_warmerhomes <- test$grants %>% dplyr::filter(date==lubridate::ymd(cal_date), grant_type=="WarmerHomes") %>% dplyr::pull(n_grant)
  n_grant <- n_oss + n_warmerhomes+n_betterenergy
  #print(n_grant)
  #
  cost_oss <- test$grants %>% dplyr::filter(date==lubridate::ymd(cal_date), grant_type=="OSS") %>% dplyr::pull(grants_Meuro)
  cost_betterenergy <- test$grants %>% dplyr::filter(date==lubridate::ymd(cal_date), grant_type=="BetterEnergyHomes") %>% dplyr::pull(grants_Meuro)
  cost_warmerhomes <- test$grants %>% dplyr::filter(date==lubridate::ymd(cal_date), grant_type=="WarmerHomes") %>% dplyr::pull(grants_Meuro)
  #
  n_b2 <- test$efficiency %>% dplyr::filter(date==lubridate::ymd(cal_date)) %>% dplyr::pull(n_b2)
  n_b2_0 <- test$efficiency %>% dplyr::filter(date==lubridate::ymd(start_date)) %>% dplyr::pull(n_b2)
  n_upgrade <- test$efficiency %>% dplyr::filter(date==lubridate::ymd(cal_date)) %>% dplyr::pull(n_b2)
  #print(n_b2_0)
  cals <- tibble::tibble(beta.=beta,eta.=eta,p.=p,nu.=nu,rho.=rho,r.=r)
  #print(cals)
  cals <- cals %>% dplyr::bind_cols(tibble::tibble(n_heat=n_heat_pump-n_heat_pump_0,n_heat_2015=n_heat_pump_0,number_b2=n_b2-n_b2_0,number_b2_2015=n_b2_0,
                                                   oss_total=cost_oss,warmerhomes_total=cost_warmerhomes,betterenergy_total=cost_betterenergy,
                                                   n_oss = n_oss,n_warmerhomes=n_warmerhomes,n_betterenergy=n_betterenergy,
                                                   n_fabric_total=n_upgrade))
  #print(cals)
  #print(cals %>% dplyr::bind_cols(tibble::tibble(betterenergy_cost=cost_betterenergy)))
  closeAllConnections()
  return(list(parameters=cals,efficiency=efficiencies,grants=grants))
  #observations 2023 60,000 households 208 MW 2024 94,000 households 373 MW
}


#calABM(sD,10,beta=params$beta.,lambda=params$lambda.,p=params$p.,nu=params$nu.,rho=params$rho.,r.=params$r.)


#' get_financial_utility_scale
#'
#' @param agents_in agent chacteristics e.g. output by initialise_agents()
#' @param cal_run calibration run in 1 to 100
#'
#' @returns a scalar (beta.)
#' @export
#'
#' @examples
get_financial_utility_scale <- function(agents_in,cal_run){

  gen_optimised_hp <- function(agents_in,n_sample=nrow(pv_survey_oo),tariff_plan="night_saver",no_grant = FALSE){

    survey_time <- 2024
    params <- scenario_params(sD,survey_time)
    if(no_grant) params$grant_removal_date <- yeartime -1 #remove grant
    #empirical_u <- empirical_utils_oo %>% dplyr::filter(calibration==cal_run)
    agents_in <- agents_in %>% dplyr::slice_sample(n=n_sample) %>% dplyr::rowwise() %>% dplyr::mutate(result = list(pvbess_optim_complex(aspect,round(area_1*params$kWp_per_m2),round(area_2*params$kWp_per_m2),shading1,shading2,D_max,D_min,params,tariff_plan=tariff_plan))) %>% tidyr::unnest_wider(result)
    agents_in %>% dplyr::select(ID,q14,q15,D_max,D_min,aspect,shading1,shading2,S_1,S_2,B,savings) %>% return()
  }

  agents_in <- gen_optimised_pvbess(agents_in)
  survey_u <- agents_in %>% dplyr::group_by(q14) %>% dplyr::summarise(savings=-median(savings))
  empirical_u <- empirical_utils_oo %>% dplyr::filter(calibration==cal_run) %>% dplyr::filter(question_code=="q14")
  empirical_u <- empirical_u %>% dplyr::select(response_code,du_average) %>% dplyr::rename("q14"=response_code)
  survey_u <- survey_u %>% dplyr::inner_join(empirical_u)
  #coef(lm(du_average~savings,survey_u))[2] %>% return()
  #IQR(survey_u$du_average)/IQR(survey_u$savings) %>% return()
  mad(survey_u$du_average)/mad(survey_u$savings) %>% return() #mean absolute deviation relative to median
}
