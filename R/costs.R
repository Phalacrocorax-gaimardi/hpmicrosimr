#sD <- readxl::read_xlsx("C:/Users/Joe/pkgs/hpmicrosimr/inst/extdata/scenario_parameters.xlsx",sheet="WEM")
#historical energy prices
#energy_prices <- readr::read_csv("C:/Users/Joe/pkgs/hpmicrosimr/inst/extdata/energy_prices.csv")
#oxy <- sinew::makeOxygen(energy_prices)
#writeLines(oxy, "R/energy_prices.R")

#tech_cost_params <- readr::read_csv("C:/Users/Joe/pkgs/hpmicrosimr/inst/extdata/technology_cost_model.csv")
#tech_efficiency_params <-  readr::read_csv("C:/Users/Joe/pkgs/hpmicrosimr/inst/extdata/tech_efficiency_parameters.csv")
#use_data(tech_efficiency_params,overwrite=T)
#tech_emissions_factors <- readr::read_csv("C:/Users/Joe/pkgs/hpmicrosimr/inst/extdata/tech_emissions_factors.csv")
#use_data(tech_emissions_factors,overwrite=T)

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
#' historical retail heating fuel prices in cents/kWh.\cr
#' \cr
#' prices reflect carbon pricing from carbon_Price_fun()
#'
#' @param fuel_type one of oil,gas,electricity,solid_fuel
#' @param sD scenario dataframe
#' @param yeartime decimal time
#'
#' @return cents per kWh
#' @export
#'
#' @examples
#'
#' energy_price_fun("oil", sD, 2025)
#' energy_price_fun("oil", sD, 2030)
#' energy_price_fun("oil", sD, 2035)
#' energy_price_fun("oil", sD, 2040)
#'
energy_price_fun <- function(fuel_type,sD,yeartime){
  #
  emissions_factors <- tech_emissions_factors %>% dplyr::mutate(gCO2_per_kWh=replace(gCO2_per_kWh,tech=="solid_fuel",ef_solid_fuel_fun(sD,yeartime)))

  stopifnot(fuel_type %in% c("oil","gas","electricity","solid_fuel"))
  prices <- energy_prices %>% dplyr::filter(fuel==fuel_type) %>% dplyr::select(-fuel)
  for(year in c(2025,2030,2035,2050)){
    prices <- prices %>% dplyr::bind_rows(tibble::tibble(year=year,price = sD %>% dplyr::filter(parameter==paste(fuel_type,"price",year,sep="_")) %>% dplyr::pull(value)))
  }
  cost <- approx(x=prices$year+0.5, y=prices$price,xout=yeartime,rule=2)$y
  #add post 2024 carbon price increment
  ef <- tech_emissions_factors %>% dplyr::filter(tech==fuel_type) %>% dplyr::pull(gCO2_per_kWh)
  if(yeartime > 2025.5) cost <- cost + ef/1e+4*(carbon_price_fun(sD,yeartime) - 63.5)
  return(cost)
}


#' carbon_price_fun
#'
#' the prevailing carbon price for residential heating fuels
#'
#' @param sD  scenario parameters
#' @param yeartime decimal time
#'
#' @returns euros per tCO2
#' @export
#'
#' @examples
#' carbon_price_fun(sD,2035)
carbon_price_fun <- function(sD,yeartime){

  prices <- sD %>% dplyr::filter(stringr::str_detect(parameter,"carbon_price")) %>% dplyr::pull(value)

  cost <- approx(x=c(2010.5,2025.5,2030.5,2040.5), y=prices,xout=yeartime,rule=2)$y
  return(cost)


}



