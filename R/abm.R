#hp_model_weights_oo <- readr::read_csv("C:/Users/joe/pkgs/hpmicrosimr/inst/extdata/hp_model_weights.csv")
#hp_empirical_utils <- readr::read_csv("C:/Users/joe/pkgs/hpmicrosimr/inst/extdata/hp_empirical_utils.csv")
#hp_survey_oo <- readr::read_csv("C:/Users/joe/pkgs/hpmicrosimr/inst/extdata/hp_survey_oo.csv")
#hp_survey_oo <- hp_survey_oo %>% dplyr::filter(!is.na(q53_5))
#hp_questions <- readr::read_csv("C:/Users/joe/pkgs/hpmicrosimr/inst/extdata/hp_questions.csv")
#hp_qanda <- readr::read_csv("C:/Users/joe/pkgs/hpmicrosimr/inst/extdata/hp_qanda.csv")
#sD <- readxl::read_xlsx("C:/Users/joe/pkgs/hpmicrosimr/inst/extdata/scenario_parameters.xlsx",sheet="WEM")
#sD_cap <- readxl::read_xlsx("C:/Users/joe/pkgs/hpmicrosimr/inst/extdata/scenario_parameters.xlsx",sheet="CAP")

#use_data(sD,overwrite=TRUE)


#' scenario_params
#'
#' builds the complete parameter set at yeartime from scenario sD
#'
#' @param scen scenario parameters e.g. scenario_0
#' @param yeartime decimal time
#'
#' @return long form dataframe containing parameter names and values
#' @export
#'
#' @examples
#' params <- scenario_params(sD,2025.5)
#'
scenario_params <- function(scen,yeartime){
  #fast params
  technologies <- c("oil","gas","electricity","heat_pump","wood")
  sc <- tibble::tibble(parameter="yeartime", value=  yeartime)
  sc <- dplyr::bind_rows(sc,tibble::tibble(parameter="hdd", value=  dplyr::filter(scen, parameter=="hdd")$value))
  sc <- dplyr::bind_rows(sc,tibble::tibble(parameter="reference temperature", value=  dplyr::filter(scen, parameter=="")$value))
  sc <- dplyr::bind_rows(sc,tibble::tibble(parameter="labour_cost", value=  labour_cost_fun(scen,yeartime)))
  sc <- dplyr::bind_rows(sc,tibble::tibble(parameter="heating_controller_cost", value=  dplyr::filter(scen, parameter=="heating_controller_cost")$value))
  sc <- dplyr::bind_rows(sc,tibble::tibble(parameter="installer_overhead",  value=dplyr::filter(scen, parameter=="overhead")$value))
  sc <- dplyr::bind_rows(sc,tibble::tibble(parameter="installer_profit_margin", value=  dplyr::filter(scen, parameter=="installer_profit_margin")$value))
  #system maintenance costs
  for(tech in technologies){
    sc <- dplyr::bind_rows(sc,tibble::tibble(parameter=paste(tech,"_system_maintenance",sep=""), value=  dplyr::filter(scen, parameter==paste(tech,"_system_maintenance",sep=""))$value))
  }
  #fuel prices
  for(fuel_type in c("oil","gas","electricity","wood")){
    sc <- dplyr::bind_rows(sc,tibble::tibble(parameter=paste(fuel_type,"price",sep="_"), value=energy_price_fun(fuel_type,scen,yeartime)  ))
  }
  #night rate vs day rate
  sc <- dplyr::bind_rows(sc,tibble::tibble(parameter="night_rate_discount", value =  night_discount_fun(scen,yeartime)))
  sc <- dplyr::bind_rows(sc,tibble::tibble(parameter="hp_night_rate_usage_factor",  value=dplyr::filter(scen, parameter=="hp_night_rate_usage_factor")$value))
  sc <- dplyr::bind_rows(sc,tibble::tibble(parameter="electric_night_rate_usage_factor",  value=dplyr::filter(scen, parameter=="electric_night_rate_usage_factor")$value))
  #space heating
  sc <- dplyr::bind_rows(sc,tibble::tibble(parameter="q_passive",  value=dplyr::filter(scen, parameter=="q_passive")$value))
  sc <- dplyr::bind_rows(sc,tibble::tibble(parameter="q_hotwater",  value=dplyr::filter(scen, parameter=="q_hotwater")$value))
  sc <- dplyr::bind_rows(sc,tibble::tibble(parameter="q_lighting",  value=dplyr::filter(scen, parameter=="q_lighting")$value))

  sc <- dplyr::bind_rows(sc,tibble::tibble(parameter="present_bias_threshold",  value=dplyr::filter(scen, parameter=="present_bias_threshold")$value))
  sc <- dplyr::bind_rows(sc,tibble::tibble(parameter="rebound_threshold",  value=dplyr::filter(scen, parameter=="rebound_threshold")$value))
  sc <- dplyr::bind_rows(sc,tibble::tibble(parameter="warmer_homes_target_ber",  value=dplyr::filter(scen, parameter=="warmer_homes_target_ber")$value))
  sc <- dplyr::bind_rows(sc,tibble::tibble(parameter="warmer_homes_cost_cap",  value=dplyr::filter(scen, parameter=="warmer_homes_cost_cap")$value))
  sc <- dplyr::bind_rows(sc,tibble::tibble(parameter="warmer_homes_enhanced",  value=dplyr::filter(scen, parameter=="warmer_homes_enhanced")$value))
  sc <- dplyr::bind_rows(sc,tibble::tibble(parameter="warmer_homes_cost_cap_enhanced",  value=dplyr::filter(scen, parameter=="warmer_homes_cost_cap_enhanced")$value))
  sc <- dplyr::bind_rows(sc,tibble::tibble(parameter="warmer_homes_target_ber_enhanced",  value=dplyr::filter(scen, parameter=="warmer_homes_target_ber_enhanced")$value))


  sc <- dplyr::bind_rows(sc,tibble::tibble(parameter="minimum_fabric_grant",  value=dplyr::filter(scen, parameter=="minimum_fabric_grant")$value))
  sc <- dplyr::bind_rows(sc,tibble::tibble(parameter="grant_increase_date",  value=dplyr::filter(scen, parameter=="grant_increase_date")$value))
  sc <- dplyr::bind_rows(sc,tibble::tibble(parameter="grant_increase_factor",  value=dplyr::filter(scen, parameter=="grant_increase_factor")$value))
  #draconian
  sc <- dplyr::bind_rows(sc,tibble::tibble(parameter="oil_boiler_ban",  value=dplyr::filter(scen, parameter=="oil_boiler_ban")$value))
  sc <- dplyr::bind_rows(sc,tibble::tibble(parameter="gas_boiler_ban",  value=dplyr::filter(scen, parameter=="gas_boiler_ban")$value))
  #sc <- dplyr::bind_rows(sc,tibble::tibble(parameter="peat_phaseout",  value=dplyr::filter(scen, parameter=="peat_phaseout")$value))

  sc <- dplyr::bind_rows(sc,tibble::tibble(parameter="carbon_price", value =  carbon_price_fun(scen,yeartime)))

  sc <- dplyr::bind_rows(sc,tibble::tibble(parameter="ber_upgrade_labour_cost_share",  value=dplyr::filter(scen, parameter=="ber_upgrade_labour_cost_share")$value))

  sc <- dplyr::bind_rows(sc,tibble::tibble(parameter="overhead",  value=dplyr::filter(scen, parameter=="overhead")$value))
  #scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="overhead",  value=dplyr::filter(sD, parameter=="vat_service")$value))
  sc <- dplyr::bind_rows(sc,tibble::tibble(parameter="vat_service",  value=dplyr::filter(scen, parameter=="vat_service")$value))
  sc <- dplyr::bind_rows(sc,tibble::tibble(parameter="vat_goods",  value=dplyr::filter(scen, parameter=="vat_goods")$value))
  #
  sc <- dplyr::bind_rows(sc,tibble::tibble(parameter="pef_electricity", value =  pef_fun(scen,yeartime)))
  #hp grant dates
  sc <- dplyr::bind_rows(sc,tibble::tibble(parameter="hp_grant_introduction",  value=dplyr::filter(scen, parameter=="hp_grant_introduction")$value))
  sc <- dplyr::bind_rows(sc,tibble::tibble(parameter="hp_grant_increase",  value=dplyr::filter(scen, parameter=="hp_grant_increase")$value))
  sc <- dplyr::bind_rows(sc,tibble::tibble(parameter="hp_grant_removal",  value=dplyr::filter(scen, parameter=="hp_grant_removal")$value))
  #ber upgrade dates
  sc <- dplyr::bind_rows(sc,tibble::tibble(parameter="warmer_homes_introduction",  value=dplyr::filter(scen, parameter=="warmer_homes_introduction")$value))
  sc <- dplyr::bind_rows(sc,tibble::tibble(parameter="better_energy_introduction",  value=dplyr::filter(scen, parameter=="better_energy_introduction")$value))
  sc <- dplyr::bind_rows(sc,tibble::tibble(parameter="oss_introduction",  value=dplyr::filter(scen, parameter=="oss_introduction")$value))

  sc <- dplyr::bind_rows(sc,tibble::tibble(parameter="hli_heat_pump_threshold",  value=dplyr::filter(scen, parameter=="hli_heat_pump_threshold")$value))
  sc <- dplyr::bind_rows(sc,tibble::tibble(parameter="hli_heat_pump_threshold_increase_date",  value=dplyr::filter(scen, parameter=="hli_heat_pump_threshold_increase_date")$value))
  sc <- dplyr::bind_rows(sc,tibble::tibble(parameter="hli_heat_pump_threshold_increase_value",  value=dplyr::filter(scen, parameter=="hli_heat_pump_threshold_increase_value")$value))


  sc <- dplyr::bind_rows(sc,tibble::tibble(parameter="warmer_homes_cost_scale",  value=dplyr::filter(scen, parameter=="warmer_homes_cost_scale")$value))
  sc <- dplyr::bind_rows(sc,tibble::tibble(parameter="warmer_homes_heat_pump",  value=dplyr::filter(scen, parameter=="warmer_homes_heat_pump")$value))


  #sc <- dplyr::bind_rows(sc,tibble::tibble(parameter="ef_wood",  value =  ef_wood_fun(scen,yeartime)))

  #scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="evening_tariff", value =  evening_tariff_fun(sD,yeartime)))
  #scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="night_tariff", value =  night_tariff_fun(sD,yeartime)))
  #scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="marginal_tax_rate", value =  dplyr::filter(sD, parameter=="marginal_tax_rate")$value))   scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="discount_rate", value =  dplyr::filter(sD, parameter=="discount_rate")$value))
  #ber upgrade cost model parameters
  sc <- dplyr::bind_rows(sc,tibble::tibble(parameter="ber_upgrade_marginal_cost_k",  value=dplyr::filter(scen, parameter=="ber_upgrade_marginal_cost_k")$value))
  sc <- dplyr::bind_rows(sc,tibble::tibble(parameter="ber_upgrade_marginal_cost_alpha",  value=dplyr::filter(scen, parameter=="ber_upgrade_marginal_cost_alpha")$value))
  sc <- dplyr::bind_rows(sc,tibble::tibble(parameter="ber_upgrade_marginal_cost_c0",  value=dplyr::filter(scen, parameter=="ber_upgrade_marginal_cost_c0")$value))

  sc <- dplyr::bind_rows(sc,tibble::tibble(parameter="nu.", value =  dplyr::filter(scen, parameter=="nu.")$value))
  sc <- dplyr::bind_rows(sc,tibble::tibble(parameter="lambda.", value =  dplyr::filter(scen, parameter=="lambda.")$value)) #probabaly set to zero
  sc <- dplyr::bind_rows(sc,tibble::tibble(parameter="p.", value =  dplyr::filter(scen, parameter=="p.")$value))
  sc <- dplyr::bind_rows(sc,tibble::tibble(parameter="r.", value =  dplyr::filter(scen, parameter=="r.")$value)) #delta = 1/(1+r)
  sc <- dplyr::bind_rows(sc,tibble::tibble(parameter="rho.", value =  dplyr::filter(scen, parameter=="rho.")$value)) #rebound no rebound is rho=0
  sc <- dplyr::bind_rows(sc,tibble::tibble(parameter="beta.", value =  dplyr::filter(scen, parameter=="beta.")$value)) #presenr bias
  sc <- dplyr::bind_rows(sc,tibble::tibble(parameter="eta.", value =  dplyr::filter(scen, parameter=="eta.")$value)) #hassle hassle-free is eta=1
  sc <- dplyr::bind_rows(sc,tibble::tibble(parameter="tau.", value =  dplyr::filter(scen, parameter=="tau.")$value)) #sludge - sludge-free is eta=1


  #return(scen)
  return(sc %>% fast_params())
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
#' @param scen scenario (usable_roof_fraction only)
#' @param yeartime start year (default 2010)
#' @param cal_run calibration run number between 1 and 100
#'
#' @return a dataframe with columns
#' @export
#'
#' @examples
#'
#' test <- initialise_agents(sD,2015,10) #%>% system.time()
#'
initialise_agents <- function(scen,yeartime=2015,cal_run=10){

  #initialise to 2015
  params <- scenario_params(scen,yeartime)
  #only has dimensions 861??
  agents <- hp_model_weights_oo %>% dplyr::filter(calibration_run==cal_run) %>% dplyr::select(-calibration_run)
  #retain minimal set of features minimial set for imputing missing data
  #qh~qb+qc2+q1+qe+q3+q4+qf+q13+qg+q58+q59_2
  hp_surv <- hp_survey_oo %>% dplyr::select(serial,qc2,q1,q2,q3,q5,q6,q11,qh,qb,qe,q4,qf,q7,q13,q121,q122,q123,q124,q125,q126,q127,q128,q129,actualage,qi,qg,q58,q59_2)
  hp_surv <- recode_survey(hp_surv)
  #bias correct ber values
  #select minimal featureset
  hp_surv <- hp_surv %>% dplyr::select(serial,qc2,q1,q2,construction_year,ber,hli,floor_area,primary_heat,heating_install_time,secondary_heat1,secondary_heat2,income,fuel_allowance)
  #if heating_install_year is later than yeartime look at earlier heating system installation, assuming the same technology was used,
  #unless it is a heat pump, in which case it is 50-50 oil gas at the moment.
  hp_surv <- hp_surv %>% dplyr::rowwise() %>% dplyr::mutate(heating_install_time = ifelse(heating_install_time <= yeartime, heating_install_time,
                      prior_install_year(heating_install_time,ifelse(primary_heat != "heat_pump",primary_heat,sample(c("oil","gas"),1)),initial_year=yeartime,params)))
  #ensure that prior install year cannot be older than construction year
  hp_surv <- hp_surv %>% dplyr::mutate(heating_install_time = pmax(construction_year,heating_install_time))

  agents <- agents %>% dplyr::inner_join(hp_surv,by="serial")  #some agents are missing!
  agents <- agents %>% dplyr::rename("region"=qc2,"house_type"=q1, "storeys"=q2)
  #remove houses built after 2015
  agents <- agents %>% dplyr::filter(construction_year < yeartime)
  agents <- agents %>% dplyr::mutate(kW=peak_heating_demand(hli,floor_area))
  agents$q52 <- 1 #assume that nobody knows a heat pump owner
  #agents$q52_up <- 1 #assume that nobody knows an upgrader
  agents$serial <- as.character(agents$serial)
  #generate missing HLI scores
  #print(range(agents$hli))
  #agents <- agents %>% dplyr::mutate(hli = ifelse(hli < 0.7,0.7,hli)) #don't allow hli values less than 0.7
  #re-compute BER
  agents <- agents %>% dplyr::mutate(ber=ber_from_hli(hli,primary_heat,heating_install_time,params))
  #annualised heating cost
  agents <- agents %>% dplyr::rowwise() %>% dplyr::mutate(eac = annualised_heating_system_cost(hli,primary_heat,
                                      heating_install_time,"new",floor_area,house_type,construction_year,params,"None"))
  agents <- agents %>% dplyr::rowwise() %>% dplyr::mutate(eac_actual = annualised_heating_system_cost(hli,primary_heat,
                                    heating_install_time,"new",floor_area,house_type,construction_year,params,"None",include_rebound = TRUE))

  #optionally remove some columns
  agents <- agents %>% dplyr::select(-secondary_heat1,-secondary_heat2)
  agents <- agents %>% dplyr::rename("tech"=primary_heat)
  #add randomised eta (disruption) values
  agents <- agents %>% dplyr::inner_join(eta_values)
  #compute eta values based on macro-calibration parameter
  agents <- agents %>% dplyr::ungroup() %>% dplyr::mutate(eta=exp(log_eta_bar + rnorm(dplyr::n(),0,0.1481237))) %>% dplyr::select(-log_eta_bar)
  agents <- agents %>% dplyr::mutate(eta = eta/mean(eta)) %>% dplyr::mutate(eta = params$eta.*eta)

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
#' @param scen  scenario dataframe
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

update_agents <- function(scen,yeartime,agents_in, social_network,ignore_social=F,cal_run=50, quiet=TRUE){
  #
  #beta. <- 0.2532785
  #params at yeartime
  params <- scenario_params(scen,yeartime)
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
  #re-evalute BER to capture PEF drift
  a_s <- a_s %>% dplyr::mutate(ber=ber_from_hli(hli,tech,heating_install_time,params))
  #calculate updated eac and eac_actual
  a_s <- a_s %>% dplyr::rowwise() %>% dplyr::mutate(eac = annualised_heating_system_cost(hli,tech,heating_install_time,"new",floor_area,house_type,construction_year,params,"None",include_rebound = FALSE))
  a_s <- a_s %>% dplyr::rowwise() %>% dplyr::mutate(eac_actual = annualised_heating_system_cost(hli,tech,heating_install_time,"new",floor_area,house_type,construction_year,params,"None",include_rebound = TRUE))
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
  #filter on system upgraders, but failures are not also upgraders
  b_s2 <- a_s %>% dplyr::filter(!failure) %>% dplyr::slice_sample(n=roundr(dim(a_s)[1]*params$p.))
  b_s2 <- b_s2 %>% dplyr::select(-heat_pump_grant)
  #print(paste("number of potential upgraders",dim(b_s2)[1]))
  #households that consider full upgrade following failure
  #b_s3 <- b_s1 %>% dplyr::filter(serial %in% b_s2$serial)
  #exclude failure where upgrade is being implemented => just two categories failure and upgr
  #b_s1 <- b_s1 %>% dplyr::filter(!(serial %in% b_s3$serial))

  if(nrow(b_s1)==0 && nrow(b_s2)==0) return(a_s)

  hp_savings_env <- function(hli,tech, house_type,storeys, construction_year, region, floor_area,eta) {
    heat_pump_savings(hli,tech, params$yeartime, house_type, storeys,construction_year, region, floor_area, eta,params)
  }
  hp_upgrade_savings_env <- function(hli,tech, heating_install_time,house_type, storeys,construction_year, region, floor_area,eta,fuel_allowance) {
    heat_pump_upgrade_savings(hli,tech, heating_install_time, house_type,storeys, construction_year, region, floor_area, eta, params,fuel_allowance,include_grants = TRUE)
  }

  ########################
  # Heating system failures
  ########################

  #logic: if has heat pump replace
  b_s0 <- b_s1 %>% dplyr::select(hli,tech,house_type, storeys,construction_year, region, floor_area,eta)
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
  if(nrow(b_s1_hp) > 0) {
    #b_s1_hp_stick <- b_s1_hp %>% dplyr::filter(savings <= 0 & hli <= params$hli_heat_pump_threshold)
    b_s1_hp_stick <- b_s1_hp %>% dplyr::filter(savings >= 0)
    #b_s1_hp_switch <- b_s1_hp %>% dplyr::filter(savings > 0 | hli > params$hli_heat_pump_threshold )  #
    b_s1_hp_switch <- b_s1_hp %>% dplyr::filter(savings < 0)
    #print("BUG??")
    b_s1_hp_stick <- b_s1_hp_stick %>% dplyr::mutate(eac=eac_stick,tech_cost=tech_cost_stick)
    b_s1_hp_switch <- b_s1_hp_switch %>% dplyr::mutate(eac=eac_switch,tech_cost=tech_cost_switch)
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
  if(nrow(b_s1_nhp) > 0) {
    b_s1_nhp <- b_s1_nhp %>% dplyr::mutate(du_fin = -params$nu.*w_q13*savings)
    b_s1_nhp <- b_s1_nhp %>% dplyr::mutate(du_social = w_q52*du_social[q52])
    b_s1_nhp <- b_s1_nhp %>% dplyr::mutate(du_theta = w_theta*theta)
    #sum and include hypothetical bias correction (default is zero)
    b_s1_nhp <- b_s1_nhp %>% dplyr::mutate(du_tot = du_fin+du_social+du_theta + params$lambda.)
    b_s1_nhp <- b_s1_nhp %>% dplyr::mutate(du_tot = dplyr::if_else(tech=="oil" & yeartime >= params$oil_boiler_ban,1,du_tot))
    b_s1_nhp <- b_s1_nhp %>% dplyr::mutate(du_tot = dplyr::if_else(tech=="gas" & yeartime >= params$gas_boiler_ban,1,du_tot))
    #COULD ASSUME A HLI THRESHOLD FOR FAILURE ADOPTERS i.e. clearly heat pump ready
    #b_s1_nhp_switch <- b_s1_nhp %>% dplyr::filter(du_tot > 0 & hli <= params$hli_heat_pump_threshold)
    b_s1_nhp_switch <- b_s1_nhp %>% dplyr::filter(du_tot > 0)
    b_s1_nhp_switch$tech <- "heat_pump"
    #b_s1_nhp_stick <- b_s1_nhp %>% dplyr::filter(is.na(du_tot) | du_tot <= 0 | hli > params$hli_heat_pump_threshold)
    b_s1_nhp_stick <- b_s1_nhp %>% dplyr::filter(is.na(du_tot) | du_tot <= 0)
    #print(paste("number of heat pump adopters",dim(b_s1_nhp_switch)[1]))
    #stickers
    b_s1_nhp_stick <- b_s1_nhp_stick %>% dplyr::mutate(eac=eac_stick,tech_cost=tech_cost_stick)
    b_s1_nhp_stick <- b_s1_nhp_stick %>% dplyr::select(-du_fin,-du_social,-du_theta,-du_tot)
    b_s1_nhp_stick$heat_pump_grant <- 0
    #b_s1_stick$adopt <- FALSE
  #switchers
    b_s1_nhp_switch <- b_s1_nhp_switch %>% dplyr::mutate(eac=eac_switch, tech_cost=tech_cost_switch)
    b_s1_nhp_switch <- b_s1_nhp_switch %>% dplyr::select(-du_fin,-du_social,-du_theta,-du_tot)
    b_s1_nhp <- b_s1_nhp_stick %>% dplyr::bind_rows(b_s1_nhp_switch) %>% dplyr::mutate(heating_install_time = params$yeartime)
  }
  b_s1_nhp <- b_s1_nhp %>% dplyr::select(-any_of("savings")) %>% dplyr::select(!dplyr::matches("switch|stick"))

  #if(dim(b_s1_hp)[1] != 0 ) stopifnot(dim(b_s1_hp_stick)[1] + dim(b_s1_hp_switch)[1] == dim(b_s1_hp)[1])
  b_s1 <- b_s1_hp %>% dplyr::bind_rows(b_s1_nhp)
  #calculate including rebound eac_actual
  b_s1 <- b_s1 %>% dplyr::select(!dplyr::matches("switch|stick"))
  #recompute BER - there will be an improvement
  if(nrow(b_s1)>0) b_s1 <- b_s1 %>% dplyr::mutate(ber=ber_from_hli(hli,tech,heating_install_time,params))

  ################################
  # Energy Efficiency upgrades
  #################################

  ######################
  # each agent chooses between (1) optimum fabric upgrade + sticking with current heating tech
  # (2) fabric upgrade and switching to heat pump
  # (3) reject upgrade do nothing
  ######################
  #print("b_s2")
  b_s2$upgrade <- TRUE
  b_s0 <- b_s2 %>% dplyr::select(hli,tech,heating_install_time,house_type,storeys, construction_year, region, floor_area,eta,fuel_allowance)
  #df <- purrr::pmap(b_s0,optimise_heat_env)
  df <- purrr::pmap(b_s0,hp_upgrade_savings_env)
  df <- do.call(rbind,df)
  b_s2 <- b_s2 %>% dplyr::bind_cols(df)
  #############################################################################################
  # households that reject efficiency upgrade because of insufficient reward financial relative to disruption
  ##############################################################################################
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
    b_s2_hp_switch <- b_s2_hp %>% dplyr::filter(savings < 0)
    b_s2_hp_stick <- b_s2_hp %>% dplyr::filter(savings >= 0)

    b_s2_hp_switch <- b_s2_hp_switch %>% dplyr::mutate(tech = "gas", heating_install_time = params$yeartime)
    b_s2_hp_stick  <- b_s2_hp_stick %>% dplyr::mutate(heating_install_time = params$yeartime)
  #
    b_s2_hp_stick <- b_s2_hp_stick %>% dplyr::select(-any_of("savings"))
    b_s2_hp_stick <- b_s2_hp_stick %>% dplyr::mutate(hli = hli_stick,eac=eac_stick,upgrade_cost=upgrade_cost_stick,
                                                          tech_cost=tech_cost_stick, grant_type=grant_type_stick,upgrade_grant=upgrade_grant_stick,heat_pump_grant=heat_pump_grant_stick)
    b_s2_hp_stick <- b_s2_hp_stick %>% dplyr::select(!dplyr::matches("switch|stick"))

    b_s2_hp_switch <- b_s2_hp_switch %>% dplyr::select(-any_of("savings"))
    b_s2_hp_switch <- b_s2_hp_switch %>% dplyr::mutate(hli = hli_switch,eac=eac_switch,upgrade_cost=upgrade_cost_switch,
                                                          tech_cost=tech_cost_switch, grant_type=grant_type_switch,upgrade_grant=upgrade_grant_switch,heat_pump_grant=heat_pump_grant_switch)
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
   b_s2_nhp <- b_s2_nhp %>% dplyr::mutate(du_tot = dplyr::if_else(tech=="oil" & yeartime >= params$oil_boiler_ban,1,du_tot))
   b_s2_nhp <- b_s2_nhp %>% dplyr::mutate(du_tot = dplyr::if_else(tech=="gas" & yeartime >= params$gas_boiler_ban,1,du_tot))

   #adopters
   b_s2_nhp_switch <- b_s2_nhp %>% dplyr::filter(du_tot > 0)
   #non-adopters
   b_s2_nhp_stick <- b_s2_nhp %>% dplyr::filter(is.na(du_tot) | du_tot <= 0)
   #clean up non-adopters e.g. remove redundant "switch" data
   b_s2_nhp_stick <- b_s2_nhp_stick %>% dplyr::select(-du_fin,-du_social,-du_theta,-du_tot,-any_of("savings"))
   b_s2_nhp_stick <- b_s2_nhp_stick %>% dplyr::mutate(hli = hli_stick,eac = eac_stick,upgrade_cost=upgrade_cost_stick,
                                                     tech_cost=tech_cost_stick, grant_type=grant_type_stick,
                                                     upgrade_grant=upgrade_grant_stick,heat_pump_grant=heat_pump_grant_stick)
   b_s2_nhp_stick <- b_s2_nhp_stick %>% dplyr::select(!dplyr::matches("switch|stick"))
   #
   #clean up adopters e.g. remove redundant "stick" data
   b_s2_nhp_switch <- b_s2_nhp_switch %>% dplyr::select(-du_fin,-du_social,-du_theta,-du_tot,-any_of("savings"))
   b_s2_nhp_switch <- b_s2_nhp_switch %>% dplyr::mutate(hli = hli_switch,eac = eac_switch,upgrade_cost=upgrade_cost_switch,
                                                     tech_cost=tech_cost_switch, grant_type=grant_type_switch,
                                                     upgrade_grant=upgrade_grant_switch,heat_pump_grant=heat_pump_grant_switch)
   b_s2_nhp_switch <- b_s2_nhp_switch %>% dplyr::select(!dplyr::matches("switch|stick"))
   b_s2_nhp_switch$tech <- "heat_pump"
   #combine adopters and non-adopters
   b_s2_nhp <- b_s2_nhp_stick %>% dplyr::bind_rows(b_s2_nhp_switch) %>% dplyr::mutate(heating_install_time = params$yeartime)
  }
  #combine all upgrades whether existing heat pump or not and update to the new kW installed capacities
  b_s2 <-  b_s2_hp %>% dplyr::bind_rows(b_s2_nhp) %>% dplyr::mutate(kW=heating_system_size(ber*floor_area))

  b_s2 <- b_s2 %>% dplyr::select(-any_of("savings")) %>% dplyr::select(!dplyr::matches("switch|stick"))
  if(nrow(b_s2) > 0 ) b_s2 <- b_s2 %>% dplyr::mutate(ber=ber_from_hli(hli,tech,heating_install_time,params))
  #stopifnot(dim(b_s2_nhp)[1] + dim(b_s2_hp)[1] == dim(b_s2)[1])
  #if(dim(b_s2_nhp)[1] != 0) stopifnot(dim(b_s2_nhp_stick)[1]+dim(b_s2_nhp_switch)[1] == dim(b_s2_nhp)[1])
  #if(dim(b_s2_hp)[1] != 0) stopifnot(dim(b_s2_hp_stick)[1]+dim(b_s2_hp_switch)[1] == dim(b_s2_hp)[1])

  #combine == dim(b_s2)[1])
  ##################################
  #combine failures and upgraders
  #################################
  b_s <- b_s1 %>% dplyr::bind_rows(b_s2) %>% dplyr::ungroup()
  #compute new bers
  #stopifnot(nrow(b_s %>% dplyr::filter(lengths(tech) != 1)) != 0)
  #print(b_s)
  #b_s <- b_s  %>% dplyr::mutate(ber=ber_from_hli(hli,tech,install_time = params$yeartime,params))

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


#' Run one replicate of the ABM
#' @noRd
run_single <- function(j, sD, year_zero = 2015, simulation_end = 2030, resample_society = FALSE, ignore_social = FALSE,quiet = TRUE, network_nu = 4.5) {

  Nt <- round((simulation_end - year_zero + 1) * 6)   # bi-monthly steps
  stopifnot(Nt >= 2)

  microcal_run <- sample(1:100, 1)
  agents_in <- initialise_agents(sD, year_zero, microcal_run)

  society <- hpmicrosimr::hp_society_oo %>%
    dplyr::filter(serial %in% agents_in$serial)
  if (resample_society) {
    # bootstrap the society's attributes but keep the serials unique
    society_new <- society[sample(nrow(society), replace = TRUE), ]
    society_new$serial <- society$serial
    society <- society_new
  }

  if (!quiet) message("Generating network for run ", j, " ....")
  social <- make_artificial_society(society, hpmicrosimr::homophily, network_nu)

  agent_ts <- vector("list", Nt)
  agent_ts[[1]] <- agents_in
  for (t in seq(2, Nt)) {
    yeartime <- year_zero + (t - 1) / 6
    agent_ts[[t]] <- update_agents(sD, yeartime, agent_ts[[t - 1]],
                                   social_network = social,
                                   ignore_social = ignore_social,
                                   cal_run = microcal_run, quiet = quiet)
  }

  dates <- lubridate::ymd(paste0(year_zero, "-01-01")) %m+% months((seq_len(Nt) - 1) * 2)
  agent_ts <- data.table::rbindlist(agent_ts, idcol = "t", fill = TRUE) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(date = dates[t], simulation = j) %>%
    dplyr::select(-t)

  degrees <- tibble::tibble(serial = social %>% tibble::as_tibble() %>% dplyr::pull(serial),
                            degree = igraph::degree(social))
  dplyr::inner_join(agent_ts, degrees, by = "serial")
}


#' Run replicates on one of three backends
#' @noRd
run_replicates <- function(Nrun, FUN, backend = c("foreach", "mclapply", "serial"),n_unused_cores = 2) {
  backend <- match.arg(backend)
  n_cores <- max(1, min(Nrun, parallel::detectCores() - n_unused_cores))
  if (n_cores == 1) backend <- "serial"
  if (backend == "mclapply" && .Platform$OS.type == "windows") backend <- "foreach"

  if (backend == "serial") {
    res <- lapply(seq_len(Nrun), FUN)
  } else if (backend == "mclapply") {
    res <- parallel::mclapply(seq_len(Nrun), FUN, mc.cores = n_cores)
    failed <- vapply(res, inherits, logical(1), what = "try-error")
    if (any(failed)) stop("mclapply replicate(s) failed: ", paste(which(failed), collapse = ", "))
  } else {
    cl <- parallel::makeCluster(n_cores)
    doParallel::registerDoParallel(cl)
    on.exit({ foreach::registerDoSEQ(); parallel::stopCluster(cl) }, add = TRUE)
    res <- foreach::foreach(j = seq_len(Nrun)) %dopar% FUN(j)
  }
  dplyr::bind_rows(res)
}




#' runABM
#'
#' Runs home energy efficiency system adoption simulation on artificial society of ~792 agents.
#' Each run is performed on an independently generated social network with randomisation from initialise_agents() and
#' with a random micro_calibration run (index 1..100)\cr
#' \cr
#' Bi-monthly \cr
#' \cr
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
runABM <- function(sD, Nrun = 1, simulation_end = 2030, resample_society = FALSE,
                   n_unused_cores = 2, use_parallel = TRUE, ignore_social = FALSE,
                   quiet = TRUE) {
  year_zero <- 2015
  get_par <- function(nm) sD %>% dplyr::filter(parameter == nm) %>% dplyr::pull(value)
  pars <- c("p.", "nu.", "rho.", "r.", "beta.", "eta.", "tau.")
  vals <- vapply(pars, get_par, numeric(1))

  print(paste("financial utility scale (nu.)=", round(vals["nu."], 2), "p.=", round(vals["p."], 4),
              "discount_rate (r)=", round(vals["r."], 2), "rebound=", round(vals["rho."], 2),
              "present bias=", round(vals["beta."], 2), "disruption=", round(vals["eta."], 3),
              "sludge=", round(vals["tau."], 3)))

  seeds <- sample.int(.Machine$integer.max, Nrun)   # per-replicate seeds
  one_run <- function(j) {
    set.seed(seeds[j])
    run_single(j, sD, year_zero, simulation_end, resample_society, ignore_social, quiet)
  }
  abm <- run_replicates(Nrun, one_run, if (use_parallel) "foreach" else "serial", n_unused_cores) %>%
    dplyr::arrange(simulation, date)

  meta <- tibble::tibble(parameter = c("Nrun", "end_year", pars),
                         value = c(Nrun, simulation_end, unname(vals)))
  list("abm" = abm, "scenario" = sD, "system" = meta)
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
#' @param cal_values named numeric vector of calibration parameters, names as in sD e.g. "nu." and "p."
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

  cal_params <- tibble::as_tibble(as.list(cal_values[c("beta.", "eta.", "p.", "nu.", "rho.", "r.")]))
  calibration_summary(abm, cal_params)
}



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

calABM2 <- function(sD, Nrun=4,n_unused_cores=2, use_parallel=T, nu=0.4,p=0.004,beta=0.8,r=0.03,eta=0.03,tau=0.02,rho=0.3,quiet=TRUE){

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

  test <- summarise_abm_cal(abm, cal_date)

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
