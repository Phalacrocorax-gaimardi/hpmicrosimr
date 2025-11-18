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

  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="ber_upgrade_labour_cost_share",  value=dplyr::filter(sD, parameter=="ber_upgrade_labour_cost_share")$value))

  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="overhead",  value=dplyr::filter(sD, parameter=="overhead")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="overhead",  value=dplyr::filter(sD, parameter=="vat_service")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="vat_service",  value=dplyr::filter(sD, parameter=="vat_service")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="vat_goods",  value=dplyr::filter(sD, parameter=="vat_goods")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="labour_cost", value =  labour_cost_fun(sD,yeartime)))
  #hp grant dates
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="hp_grant_introduction",  value=dplyr::filter(sD, parameter=="hp_grant_introduction")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="hp_grant_increase",  value=dplyr::filter(sD, parameter=="hp_grant_increase")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="hp_grant_removal",  value=dplyr::filter(sD, parameter=="hp_grant_removal")$value))
  #ber upgrade dates
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="warmer_homes_introduction",  value=dplyr::filter(sD, parameter=="warmer_homes_introduction")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="better_energy_introduction",  value=dplyr::filter(sD, parameter=="better_energy_introduction")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="oss_introduction",  value=dplyr::filter(sD, parameter=="oss_introduction")$value))

  #scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="evening_tariff", value =  evening_tariff_fun(sD,yeartime)))
  #scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="night_tariff", value =  night_tariff_fun(sD,yeartime)))
  #scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="marginal_tax_rate", value =  dplyr::filter(sD, parameter=="marginal_tax_rate")$value))   scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="discount_rate", value =  dplyr::filter(sD, parameter=="discount_rate")$value))
  #ber upgrade cost model parameters
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="ber_upgrade_marginal_cost_k",  value=dplyr::filter(sD, parameter=="ber_upgrade_marginal_cost_k")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="ber_upgrade_marginal_cost_alpha",  value=dplyr::filter(sD, parameter=="ber_upgrade_marginal_cost_alpha")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="ber_upgrade_marginal_cost_c0",  value=dplyr::filter(sD, parameter=="ber_upgrade_marginal_cost_c0")$value))

  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="beta.", value =  dplyr::filter(sD, parameter=="beta.")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="lambda.", value =  dplyr::filter(sD, parameter=="lambda.")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="p.", value =  dplyr::filter(sD, parameter=="p.")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="delta.", value =  dplyr::filter(sD, parameter=="delta.")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="r.", value =  dplyr::filter(sD, parameter=="r.")$value)) #rebound


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
#' initialise_agents(sD,2015,10) %>% system.time()
#'
initialise_agents <- function(sD,yeartime=2015,cal_run){

  #initialise to 2015
  params <- scenario_params(sD,yeartime)
  #only has dimensions 861??
  agents <- hp_model_weights_oo %>% dplyr::filter(calibration_run==cal_run) %>% dplyr::select(-calibration_run)
  #retain minimal set of features minimial set for imputing missing data
  hp_surv <- hp_survey_oo %>% dplyr::select(serial,qc2,q1,q2,q3,q5,q6,q11,qh,qb,qe,q4,qf,q7,q13,q121,q122,q123,q124,q125,q126,q127,q128,q129,actualage,qi)
  hp_surv <- recode_survey(hp_surv,params)

  #select minimal featureset
  hp_surv <- hp_surv %>% dplyr::select(serial,qc2,q1,q2,construction_year,ber,floor_area,primary_heat,heating_install_time,secondary_heat1,secondary_heat2,income,fuel_allowance)

  #if heating_install_year is later than yeartime look at earlier heating system installation, assuming the same technology was used,
  #unless it is a heat pump, in which case it is 50-50 oil gas at the moment.
  hp_surv <- hp_surv %>% dplyr::rowwise() %>% dplyr::mutate(heating_install_time = ifelse(heating_install_time <= yeartime, heating_install_time,
                      prior_install_year(heating_install_time,ifelse(primary_heat != "heat_pump",primary_heat,sample(c("oil","gas"),1)),initial_year=yeartime,params)))
  #ensure that prior install year cannot be older than construction year
  hp_surv <- hp_surv %>% dplyr::mutate(heating_install_time = pmax(construction_year,heating_install_time))
  #agents <- agents %>% dplyr::inner_join(pv_survey_oo %>% dplyr::select(ID,housecode,region,q1))
  agents <- agents %>% dplyr::inner_join(hp_surv,by="serial")  #some agents are missing!
  agents <- agents %>% dplyr::rename("region"=qc2,"house_type"=q1)
  #remove houses built after 2015
  agents <- agents %>% dplyr::filter(construction_year < yeartime)
  agents <- agents %>% dplyr::mutate(kW=heating_system_size(ber*floor_area))
  agents$q52 <- 1 #assume that nobody knows a heat pump owner
  agents$serial <- as.character(agents$serial)
  #annualised heating cost
  agents <- agents %>% dplyr::rowwise() %>% dplyr::mutate(annual_cost = annualised_heating_system_cost(primary_heat,
                                      heating_install_time,"new",ber,floor_area,house_type,construction_year,"None",params))
  agents$heat_pump_grant <- 0
  agents$upgrade_grant <- 0

  return(agents %>% dplyr::ungroup())
}

#agents_in <- initialise_agents(sD,2015,100)
#social_network <- make_artificial_society(hp_society_oo %>% dplyr::filter(serial %in% agents_in$serial),homophily)

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
#update_agents(sD,2024,agents_in,social_network,cal_run=10,quiet=FALSE)
update_agents <- function(sD,yeartime,agents_in, social_network,ignore_social=F,cal_run, quiet=TRUE){

  #
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
  a_s$upgrade <- FALSE
  a_s$failure <- FALSE
  a_s$adopt <- FALSE
  a_s$upgrade_grant <-0
  a_s$heat_pump_grant <- 0
  #a_s$savings <- NA
  #update definitions of old and new for all agents
  #a_s <- a_s %>% dplyr::mutate(S1_old=S1_new,S2_old = S2_new,B_old=B_new)
  #a_s <- a_s %>% dplyr::mutate(capex_old=capex_new,opex_old=opex_new)
  #this subsample of agents decide to look at rooftop pv
  #assing any heating tech breakdowns during timestep
  a_s <- a_s %>% dplyr::mutate(failure = weibull_failure(heating_install_time,params$yeartime,params$yeartime+1/6,tech = primary_heat))
  a_s <- dplyr::ungroup(a_s)
  #filter on failure
  b_s1 <- a_s %>% dplyr::filter(failure)
  print(paste("number of heating system failures",dim(b_s1)[1]))
  b_s1 <- b_s1 %>% dplyr::select(-heat_pump_grant)
  #filter on system upgraders
  b_s2 <- dplyr::slice_sample(a_s,n=roundr(dim(a_s)[1]*params$p.))
  b_s2 <- b_s2 %>% dplyr::select(-heat_pump_grant)
  print(paste("number of potential upgraders",dim(b_s2)[1]))
  #households that consider full upgrade following failure
  b_s3 <- b_s1 %>% dplyr::filter(serial %in% b_s2$serial)
  #exclude failure where upgrade is being implemented
  b_s1 <- b_s1 %>% dplyr::filter(!(serial %in% b_s3$serial))
  print(paste("number of heating system failures coinciding with upgrades",dim(b_s3)[1]))
  #assume that all households who consider ber upgrade also consider heating system upgrade.
  #
  #redefine financial returns excluding params dependency
  optimise_heat_env <- function(ber,primary_heat, heating_install_time, house_type, construction_year, region, floor_area) {
    optimise_heat(ber,primary_heat, heating_install_time, house_type, construction_year, region, floor_area, params)
  }
  optimise_upgrade_env <- function(ber, install_time, house_type, construction_year, region, floor_area,is_fuel_allowance) {
    optimise_upgrade(ber, tech_old, install_time, house_type, construction_year, region, floor_area, params,is_fuel_allowance)
  }
  hp_savings_env <- function(ber,primary_heat, house_type, construction_year, region, floor_area) {
    heat_pump_savings(ber,primary_heat, params$yeartime, house_type, construction_year, region, floor_area, params)
  }
  hp_upgrade_savings_env <- function(ber,primary_heat, house_type, construction_year, region, floor_area,fuel_allowance) {
    heat_pump_upgrade_savings(ber,primary_heat, params$yeartime, house_type, construction_year, region, floor_area, params,fuel_allowance)
  }
  optimise_upgrade_stick <- function(ber,primary_heat,heating_install_time, house_type, construction_year, region, floor_area,fuel_allowance) {
    optimise_upgrade(ber,primary_heat, heating_install_time, house_type, construction_year, region, floor_area, params,fuel_allowance) %>% dplyr::filter(tech_new==primary_heat)
  }
  #logic: if has heat pump replace
  b_s0 <- b_s1 %>% dplyr::select(ber,primary_heat,house_type, construction_year, region, floor_area)
  #df <- purrr::pmap(b_s0,optimise_heat_env)
  df <- purrr::pmap(b_s0,hp_savings_env)
  df <- do.call(rbind,df)
  b_s1 <- b_s1 %>% dplyr::bind_cols(df)

  #reject heat pump adoption when financial utility does not overcome barriers
  b_s1 <- b_s1 %>% dplyr::mutate(du_fin = -params$beta.*w_q13*savings)
  b_s1 <- b_s1 %>% dplyr::mutate(du_social = dplyr::if_else(tech_new=="heat_pump" & primary_heat != "heat_pump",w_q52*du_social[q52],0))
  b_s1 <- b_s1 %>% dplyr::mutate(du_theta = dplyr::if_else(tech_new=="heat_pump" & primary_heat != "heat_pump",w_theta*theta,0))
  #sum and include hypothetical bias correction (default is zero)
  b_s1 <- b_s1 %>% dplyr::mutate(du_tot = du_fin+du_social+du_theta + params$lambda.)
  #
  b_s1_switch <- b_s1 %>% dplyr::filter(du_tot > 0)
  b_s1_stick <- b_s1 %>% dplyr::filter(is.na(du_tot) | du_tot <= 0)
  #
  stopifnot(dim(b_s1_stick)[1]+dim(b_s1_switch)[1] == dim(b_s1)[1])
  #stickers
  b_s1_stick <- b_s1_stick %>% dplyr::rename(new_annual_cost=stick_cost)
  b_s1_stick <- b_s1_stick %>% dplyr::select(-du_fin,-du_social,-du_theta,-du_tot,-switch_cost,-savings)
  b_s1_stick$adopt <- FALSE
  #switchers
  b_s1_switch <- b_s1_switch %>% dplyr::rename(new_annual_cost=switch_cost) %>% dplyr::select(-du_fin,-du_social,-du_theta,-du_tot,-stick_cost,-savings)
  b_s1_switch$adopt <- TRUE
  #b_s1 <- b_s1 %>% dplyr::mutate(upgrade=TRUE,result = purrr::pmap(list(ber,primary_heat,heating_install_time,
  #                                                                                                         house_type,construction_year,
  #
  b_s1 <- b_s1_stick %>% dplyr::bind_rows(b_s1_switch) %>% dplyr::mutate(heating_install_time = params$yeartime, annual_cost=new_annual_cost) %>% dplyr::select(-new_annual_cost)


  #b_s2 %>% dplyr::mutate(upgrade=TRUE,result = purrr::pmap(list(ber,primary_heat,heating_install_time,
   #                                                                             house_type,construction_year,
    #                                                                          region,floor_area,fuel_allowance),optimise_upgrade_env)) %>% tidyr::unnest_wider(result)
  #optimum upgrades
  b_s2$upgrade <- TRUE
  b_s0 <- b_s2 %>% dplyr::select(ber,primary_heat,house_type, construction_year, region, floor_area,fuel_allowance)
  #df <- purrr::pmap(b_s0,optimise_heat_env)
  df <- purrr::pmap(b_s0,hp_upgrade_savings_env)
  df <- do.call(rbind,df)
  b_s2 <- b_s2 %>% dplyr::bind_cols(df)
  #check whether financial rewards of heat pumps are sufficient
  b_s2 <- b_s2 %>% dplyr::mutate(du_fin = -params$beta.*w_q13*savings)
  b_s2 <- b_s2 %>% dplyr::mutate(du_social = dplyr::if_else(tech_new=="heat_pump" & primary_heat != "heat_pump",w_q52*du_social[q52],0))
  b_s2 <- b_s2 %>% dplyr::mutate(du_theta = dplyr::if_else(tech_new=="heat_pump" & primary_heat != "heat_pump",w_theta*theta,0))
  #sum and include a possible hypothetical bias correction (default is zero)
  b_s2 <- b_s2 %>% dplyr::mutate(du_tot = du_fin+du_social+du_theta + params$lambda.)
  #
  b_s2_switch <- b_s2 %>% dplyr::filter(du_tot > 0)
  b_s2_stick <- b_s2 %>% dplyr::filter(is.na(du_tot) | du_tot <= 0)
  #
  stopifnot(dim(b_s2_stick)[1]+dim(b_s2_switch)[1] == dim(b_s2)[1])
  #stickers
  b_s2_stick <- b_s2_stick %>% dplyr::rename(new_annual_cost=stick_cost)
  b_s2_stick <- b_s2_stick %>% dplyr::select(-du_fin,-du_social,-du_theta,-du_tot,-switch_cost,-savings)
  b_s2_stick$adopt <- FALSE
  #switchers
  b_s2_switch <- b_s2_switch %>% dplyr::rename(new_annual_cost=switch_cost) %>% dplyr::select(-du_fin,-du_social,-du_theta,-du_tot,-stick_cost,-savings)
  b_s2_switch$adopt <- TRUE
  #b_s1 <- b_s1 %>% dplyr::mutate(upgrade=TRUE,result = purrr::pmap(list(ber,primary_heat,heating_install_time,
  #add addition info                                                                                                         house_type,construction_year,
  b_s0 <- b_s2_stick %>% dplyr::select(ber,primary_heat,heating_install_time, house_type, construction_year, region, floor_area,fuel_allowance)
  df <- purrr::pmap(b_s0,optimise_upgrade_stick)
  df <- do.call(rbind,df)
  b_s2_stick <- b_s2_stick %>% dplyr::bind_cols(df)

  b_s2 <- b_s2_stick %>% dplyr::bind_rows(b_s2_switch) %>% dplyr::mutate(heating_install_time = params$yeartime, annual_cost=new_annual_cost) %>% dplyr::select(-new_annual_cost)

  #combine
  b_s <- b_s1 %>% dplyr::bind_rows(b_s2)
  #update agents
  a_s <- dplyr::filter(a_s, !(serial %in% b_s$serial))
  a_s <- dplyr::bind_rows(a_s,b_s) %>% dplyr::arrange(serial)
  a_s <- a_s %>% dplyr::mutate(primary_heat=ifelse(adopt,"heat_pump",primary_heat))
  a_s <- a_s %>% dplyr::mutate(kW=heating_system_size(ber*floor_area))
  #recompute social variable
  ma <- igraph::as_adjacency_matrix(social_network)
  g <- social_network %>% tidygraph::activate(nodes) %>% dplyr::left_join(a_s,by="serial")
  #social network conformity effect
  adopter_nodes <- igraph::V(g)$adopt==TRUE
  a_s$q52 <- as.numeric(ma %*% adopter_nodes) #social reinforcement 0 no adoption 1 adoption
  if(ignore_social) a_s$qsp52 <- 0 #no adopters assumed present in local network
  a_s <- a_s %>% dplyr::rowwise() %>% dplyr::mutate(q52 = min(q52+1,4)) #q52 encoding 1,2,3,4
  #agents_out <- a_s
  #a_s <- a_s %>% dplyr::select(-du_tot)
  if(!quiet) {
    print(paste("time", round(yeartime,1), "number of heat pump adopters following system breakdown",dim(a_s %>% dplyr::filter( (!upgrade & adopt)))[1]))
    print(paste("number of heat pump adopters following efficiency upgrade",dim(a_s %>% dplyr::filter( (upgrade & adopt)))[1]))
  }
  return(dplyr::ungroup(a_s))
}

