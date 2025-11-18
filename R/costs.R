#sD <- readxl::read_xlsx("C:/Users/Joe/pkgs/hpmicrosimr/inst/extdata/scenario_parameters.xlsx",sheet="WEM")
#historical energy prices
#energy_prices <- readr::read_csv("C:/Users/Joe/pkgs/hpmicrosimr/inst/extdata/energy_prices.csv")
#oxy <- sinew::makeOxygen(energy_prices)
#writeLines(oxy, "R/energy_prices.R")

#tech_cost_params <- readr::read_csv("C:/Users/Joe/pkgs/hpmicrosimr/inst/extdata/technology_cost_model.csv")
#tech_efficiency_params <-  readr::read_csv("C:/Users/Joe/pkgs/hpmicrosimr/inst/extdata/tech_efficiency_parameters.csv")


#' tech_params_fun
#'
#' utility function to generate tech_params environment object. The model can be adjusted by modifying the tech_cost_params dataframe,
#' loaded by this function. tech_params_fun does not have an argument at present because costs are assumed to be fixed. In future
#' a sD, yeartime argument may be added.
#'
#'
#' @return long form dataframe containing tech parameter names and values
#' @export
#'
#' @examples tech_params_fun()
tech_params_fun <- function(){

  #tech_cost_params <- readr::read_csv("C:/Users/Joe/pkgs/hpmicrosimr/inst/extdata/technology_cost_model.csv")
  #tech_efficiency_params <- readr::read_csv("C:/Users/Joe/pkgs/hpmicrosimr/inst/extdata/tech_efficiency_parameters.csv")
  data("tech_cost_params", envir = environment())
  data("tech_efficiency_params", envir = environment())
  data("tech_failure_params", envir=environment())
  tech_cost_params1 <- tech_cost_params[,c(-3,-10,-11,-12)]
  #fast params
  costs <- tech_cost_params1 %>% tidyr::pivot_longer(cols=c(-technology,-installation), names_to="variable",values_to="value")
  #fixed tech costs
  costs <- costs %>% dplyr::mutate(parameter=paste(technology,variable,installation,sep="_"))
  costs <- costs %>% dplyr::select(-technology,-installation,-variable) %>% dplyr::arrange(parameter)
  costs <- costs %>% dplyr::bind_rows(tech_efficiency_params)
  costs <- costs %>% dplyr::bind_rows(tech_failure_params)
  #return(scen)
  return(costs %>% fast_params())

}


tech_params <- tech_params_fun()


#' night_discount_fun
#'
#' Night rate discount relative to standard electricity tariff
#'
#' @param sD scenario dataframe
#' @param yeartime decimal time
#'
#' @return price per kWh in euros
#' @export
#'
#' @examples
night_discount_fun <- function(sD,yeartime){
  #night_discount_fun(sD,2027)
  values <- sD %>% dplyr::filter(stringr::str_detect(parameter,"night_rate_discount")) %>% dplyr::pull(value)
  approx(x=c(2015.5,2025.5,2035.5,2050.5), y=values,xout=yeartime,rule=2)$y %>% return()
}


#' labour_cost_fun
#'
#' heating engineer labour cost per hour
#'
#' @param sD scenario dataframe
#' @param yeartime decimal time
#'
#' @return euros
#' @export
#'
#' @examples
#'
#' labour_cost_fun(sD,2018.7)
#'
labour_cost_fun <- function(sD,yeartime){
  #
  values <- sD %>% dplyr::filter(stringr::str_detect(parameter,"labour_cost_20")) %>% dplyr::pull(value)
  cost <- approx(c(2005.5,2015.5,2025.5,2035.5,2050.5), y=values,xout=yeartime,rule=2)$y
  return(cost)
}


#' energy_price_fun
#'
#' energy price model
#'
#' @param fuel_type type (oil, gas, )
#' @param sD scenario dataframe
#' @param yeartime decimal time
#'
#' @return euros
#' @export
#'
#' @examples energy_price_fun("oil", sD, 2036)
energy_price_fun <- function(fuel_type,sD,yeartime){
  #
  stopifnot(fuel_type %in% c("oil","gas","electricity","solid_fuel"))
  prices <- energy_prices %>% dplyr::filter(fuel==fuel_type) %>% dplyr::select(-fuel)
  for(year in c(2025,2030,2035,2050)){
    prices <- prices %>% dplyr::bind_rows(tibble::tibble(year=year,price = sD %>% dplyr::filter(parameter==paste(fuel_type,"price",year,sep="_")) %>% dplyr::pull(value)))
  }
  cost <- approx(x=prices$year+0.5, y=prices$price,xout=yeartime,rule=2)$y
  return(cost)
}


heat_pump_installation_grant <- function(sD, yeartime, q1,q5) {

  # Eligibility for heat pumps__before 2021
  # Simplify the HP type hp_type == "air to air" & built_year <= 2021 ~ 3500
  hp_grant <- dplyr::case_when(
    # built_year > 2021
    q5==6 ~ 0,
    # building_type == "apartment" & built_year <= 2021
    q5 %in% c(1:5) & q1==1 ~ 4500,
    # building_type != "apartment" & built_year <= 2021
    q1 != 1 & q5 %in% c(1:5)~ 6500
  )

  npv_hp_grant <- PVIF(r, heating_upgrade_time) * hp_grant # + (retrofit_cost - retrofit_grant))

  return(npv_hp_grant)
}


