#sD <- readxl::read_xlsx("C:/Users/Joe/pkgs/hpmicrosimr/inst/extdata/scenario_parameters.xlsx",sheet="WEM")
#historical energy prices
#energy_prices <- readr::read_csv("C:/Users/Joe/pkgs/hpmicrosimr/inst/extdata/energy_prices.csv")
#oxy <- sinew::makeOxygen(energy_prices)
#writeLines(oxy, "R/energy_prices.R")

#tech_cost_params <- readr::read_csv("C:/Users/Joe/pkgs/hpmicrosimr/inst/extdata/technology_cost_model.csv")


#' tech_params_fun
#'
#' utility function to generate tech_params environment object. This can be modified through tech_cost_params dataframe,
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
  #tech_cost_params <- tech_cost_params[,c(-3,-10,-11,-12)] %>% dplyr::mutate(technology= gsub(" ","_",technology))
  #fast params
  costs <- tech_cost_params %>% tidyr::pivot_longer(cols=c(-technology,-installation), names_to="variable",values_to="value")
  #fixed tech costs
  costs <- costs %>% dplyr::mutate(parameter=paste(technology,variable,installation,sep="_"))
  costs <- costs %>% dplyr::select(-technology,-installation,-variable) %>% dplyr::arrange(parameter)
  #return(scen)
  return(costs %>% fast_params())
}


#tech_params <- tech_params_fun()
#use_data(tech_params,overwrite=T)


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
#' @examples labour_cost_fun(sD,2018.7)
labour_cost_fun <- function(sD,yeartime){
  #
  values <- sD %>% dplyr::filter(stringr::str_detect(parameter,"labour_cost")) %>% dplyr::pull(value)
  cost <- approx(c(2015.5,2025.5,2035.5,2050.5), y=values,xout=yeartime,rule=2)$y
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


#' heat_pump_grant
#'
#' The fast version of seai_grant
#'
#' @param house_type house type
#' @param construction_year year
#' @param params  current parameters
#'
#' @return grant amount in euros
#' @export
#'
#' @examples heat_pump_grant("apartment",2003,scenario_params(sD,2025))
heat_pump_grant <- function(house_type,construction_year,params) {
  # Return grant amount based on date and type
  stopifnot(house_type %in% c("detached","semi_detached","terraced","apartment"))
  hp_house_type <- ifelse(house_type=="apartment","apartment","house")
  if (params$yeartime < params$hp_grant_introduction | params$yeartime > params$hp_grant_removal | construction_year > 2020) {
    return(0)  # No grant before Q2 2018
  } else if (params$yeartime < params$hp_grant_increase) {
    # Original grant scheme: flat rates, same for all dwelling types
      return(3500)
    }
    else {
    # Post-Feb 2022 scheme: distinction by dwelling type
      return(ifelse(hp_house_type == "house", 6500, 4500))
    }
}


#library(data.table)

# Define grant lookup table
get_hp_grant_table <- function(params){

grant_table <- data.table(
  from_date = c(params, 2022.15),
  to_date   = c(2022.15, 2030),
  house          = c(3500, 6500),
  apartment      = c(3500, 4500)
)
}

# Fast grant lookup function
heat_pump_grant_fast <- function(yeartime, house_type) {
  stopifnot(house_type %in% c("house", "apartment"))

  # Use binary search-style match
  idx <- which(decimal_date >= grant_table$from_date & decimal_date < grant_table$to_date)
  if (length(idx) == 0) return(0)

  return(grant_table[[house_type]][idx])
}


