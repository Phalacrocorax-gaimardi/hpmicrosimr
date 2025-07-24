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
  #system lifetimes
  for(tech in technologies){
    scen <- dplyr::bind_rows(scen,tibble::tibble(parameter=paste(tech,"_system_lifetime",sep=""), value=  dplyr::filter(sD, parameter==paste(tech,"_system_lifetime",sep=""))$value))
  }
  #system efficiencies
  for(tech in technologies){
    scen <- dplyr::bind_rows(scen,tibble::tibble(parameter=paste(tech,"_system_efficiency",sep=""), value=  dplyr::filter(sD, parameter==paste(tech,"_system_efficiency",sep=""))$value))
  }
  #system maintenance
  for(tech in technologies){
    scen <- dplyr::bind_rows(scen,tibble::tibble(parameter=paste(tech,"_system_maintenance",sep=""), value=  dplyr::filter(sD, parameter==paste(tech,"_system_maintenance",sep=""))$value))
  }

  #fuel prices
  for(fuel_type in c("oil","gas","electricity","solid_fuel")){
    scen <- dplyr::bind_rows(scen,tibble::tibble(parameter=paste(fuel_type,"price",sep="_"), value=energy_price_fun(fuel_type,sD,yeartime)  ))
  }

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
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="beta.", value =  dplyr::filter(sD, parameter=="beta.")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="lambda.", value =  dplyr::filter(sD, parameter=="lambda.")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="p.", value =  dplyr::filter(sD, parameter=="p.")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="delta.", value =  dplyr::filter(sD, parameter=="delta.")$value))


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
#' @examples initialise_agents(sD,2015,10)
initialise_agents <- function(sD,yeartime,cal_run){

  #initialise to 2015
  #params <- scenario_params(sD,yeartime)
  agents <- hp_model_weights_oo %>% dplyr::filter(calibration_run==cal_run) %>% dplyr::select(-calibration_run)
  #retain minimal set of features minimial set for imputing missing data
  hp_surv <- hp_survey_oo %>% dplyr::select(serial,qc2,q1,q2,q3,q5,q6,q11,qh,qb,qe,q4,qf,q7,q13,q121,q122,q123,q124,q125,q126,q127,q128,q129,)
  hp_surv <- recode_survey(hp_surv)
  #select minimal feature set
  hp_surv <- hp_surv %>% dplyr::select(serial,qc2,q1,q2,q3,q5,q11,q6,ber,ground_floor_area,income,primary_heat,secondary_heat1,secondary_heat2)
  #agents <- agents %>% dplyr::inner_join(pv_survey_oo %>% dplyr::select(ID,housecode,region,q1))
  agents <- agents %>% dplyr::inner_join(hp_surv,by="serial")

  return(agents %>% dplyr::ungroup())
}

#init_agents <- initialise_agents(sD,2024,100)
