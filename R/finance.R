#load params and tech_params to run examples
#params <- scenario_params(sD,2026)
#tech_params <- tech_params_fun()



# finance functions for hpmicrosimr
#
#' crf
#'
#' capital reduction factor
#'
#' @param r interest rate or time preference, decimal
#' @param term term of loan or investment
#'
#' @return real value
#' @export
#'
#' @examples crf(0.03,15)
crf <- function(r,term){
  #amortisation payment (annuity)
  res <- dplyr::case_when(r != 0~(r*(1+r)^term)/((1+r)^term-1),
                          r == 0~1/term)
  return(res)
}


#' heating_system_size
#'
#' returns the kW heating system required to heat building (rounded up to nearest kW).
#'
#' @param annual_heating_requirement estimated annual heating requirement based on BER and floor area
#' @param hdd_annual annual Heating Degree Days at location
#' @param coldest_day mean temperature expected on the coldest day (design temperature e.g -2C)
#' @param operating_hours assumed max operating hours
#' @param efficiency boiler efficiency
#'
#' @returns kW capacity
#' @export
#'
#' @examples heating_system_size(200*200)
heating_system_size <- function(annual_heating_requirement, hdd_annual=2200, coldest_day=-5,operating_hours=12, efficiency=0.9) {

  #average heating required per degree day
  kwh_per_hdd <- annual_heating_requirement / hdd_annual

  hdd_worst_day <- 16-coldest_day
  #
  kw_peak <- kwh_per_hdd * hdd_worst_day/24
  #assume system runs for 1 hours per day on coldest days
  return(ceiling(24/operating_hours*kw_peak/efficiency))
}

#' heating_system_capital_cost
#'
#' cost of a new heating system. Includes VAT using the "two-thirds" rule.
#'
#' The current version of this function assumed that ancilliary costs are halved if the same technology is being replaced.
#'
#' The heat pump grant is included where eligible by default.
#'
#' @param tech primary heating technology
#' @param kW system capacity
#' @param house_type seai house type
#' @param construction_year year of construction, integer
#' @param installation "new" or "swap"
#' @param params current cost parameters output from scenario_params
#' @param include_vat TRUE/FALSE
#' @param include_grant TRUE/FALSE
#'
#' @returns euro cost
#' @export
#'
#' @examples
heating_system_capital_cost <- function(tech,kW,house_type,construction_year,installation = "swap", params, include_vat = TRUE,include_grant=FALSE){
  #heating_system_capital_cost("heat_pump",18,"semi_detached","swap",params,include_grant=T)
  stopifnot(tech %in% c("heat_pump","gas","oil","electricity","solid_fuel"))
  stopifnot(house_type %in% c("detached","semi_detached","terraced","apartment"))
  #house_type <- ifelse(house_type=="apartment","apartment","house")
  #tech1 <- gsub(" ","_",tech)

  cost_service <- (tech_params[[paste(tech,"fixed_hours",installation,sep="_")]] + tech_params[[paste(tech,"kw_hours",installation,sep="_")]]*kW)*params$labour_cost
  cost_goods <- tech_params[[paste(tech,"fixed_tech_cost",installation,sep="_")]] + tech_params[[paste(tech,"fixed_ancilliary_cost",installation,sep="_")]]
  cost_goods <- cost_goods + kW*(tech_params[[paste(tech,"kw_ancilliary_cost",installation,sep="_")]] + tech_params[[paste(tech,"kw_tech_cost",installation,sep="_")]])
  #cost include ancilliary costs of not replacing an existing system
  cost <- cost_goods+cost_service

  cost - ifelse(tech=="heat_pump" & installation=="new" & include_grant, heat_pump_grant(house_type,construction_year,params),0)
}

#' annualised_heating_system_cost
#'
#' annualised capex for heating system installation. used a calibration discount rate from params
#'
#' @param tech heating technology
#' @param kW system capacity
#' @param house_type seai house type
#' @param construction_year year, integer
#' @param installation "swap" or "new"
#' @param params parameters output from scenario_params()
#' @param include_grant TRUE/FALSE
#'
#' @returns euro amount
#' @export
#'
#' @examples
annualised_capex <- function(tech,kW,house_type,construction_year,installation="swap",params,include_grant=TRUE){
  #annualised_capex("gas",24,"semi_detached","swap",params,include_grant=TRUE)
  heating_system_capital_cost(tech,kW,house_type,construction_year,installation,params,include_grant = include_grant)*crf(params$delta.,params[[paste(tech,"_system_lifetime",sep="")]])
}

#tech_fuel_dictionary <- c("electricity"="electricity","heat_pump"= "electricity","solid_fuel"="solid_fuel","gas"="gas","oil"="oil")
#house_type_dictionary <- c("Flat or apartment"="apartment","Terraced house"="terraced","Semi-detached house"="semi_detached","Detached house"="detached","Other"="detached")

#' heating_system_operating cost
#'
#' cost based annual fuel consumption inferred from kWh heating_requirement. Also includes an annual maintenance cost.
#'
#' @param tech heating technology
#' @param heating_requirement annual heating requirement in kWh
#' @param params current parameters
#'
#' @returns
#' @export
#'
#' @examples
heating_system_operating_cost <- function(tech, heating_requirement,params){
  #heating_system_operating_cost("electricity",150*125,params)
  tech_fuel_dictionary <- c("electricity"="electricity","heat_pump"= "electricity","solid_fuel"="solid_fuel","gas"="gas","oil"="oil")
  fuel_type <- tech_fuel_dictionary[tech]
  opex <- heating_requirement*params[[paste(fuel_type,"price",sep="_")]]/(100*params[[paste(tech,"system_efficiency",sep="_")]]) #prices given in cents per kWh
  opex + params[[paste(tech,"system_maintenance",sep="_")]]
}

#' annualised_heating_system_cost
#'
#' @param tech heating technology
#' @param installation new or swap
#' @param heating_requirement inferred heating requirement
#' @param house_type housing type (q1)
#' @param construction_year year, integer
#' @param params parameter values at yeartime
#' @param include_grant TRUE/FALSE default TRUE
#'
#' @returns
#' @export
#'
#' @examples
annualised_heating_system_cost <- function(tech,installation="swap",heating_requirement,house_type,construction_year,params,include_grant=FALSE){
   #annualised_heating_system_cost("heat_pump","new",125*150,"semi_detached",2003,params,include_grant=T)
   stopifnot(tech %in% c("solid_fuel","gas","oil","heat_pump","electricity"))
   stopifnot(installation %in% c("swap","new"))
   #specify heating system size
   kW <- heating_system_size(heating_requirement)
   #house_type <- ifelse(q1=="Flat or apartment","apartment","house")
   capex <- heating_system_capital_cost(tech,kW,house_type,construction_year,installation,params,include_grant = include_grant)
   #tech,kW,house_type,installation = "swap", params, include_vat = TRUE,include_grant=TRUE
   #print(paste("capex=",capex))
   #if(include_grant & tech=="heat_pump") {grant <- heat_pump_grant(house_type,params)
     #                 capex <- capex - grant}
   opex <- heating_system_operating_cost(tech,heating_requirement,params)
   #print(paste("opex=",opex))

   capex*crf(params$delta.,params[[paste(gsub(" ","_",tech),"_system_lifetime",sep="")]]) + opex

}

#' is_eligible_fuel_allowance
#'
#' estimates whether the household is entitled to fuel allowance. Is so, the household qualifies for
#' the warmer homes scheme
#'
#' @param age actual age
#' @param income actual income (imputed)
#' @param qi household profile
#'
#' @returns TRUE/FALSE
#' @export
#'
#' @examples
is_eligible_fuel_allowance <- function(age, income,qi) {
  #is_eligible_fuel_allowance(70,30000,2)
  stopifnot(qi %in% 1:9)
  if(!(qi %in% c(1:5))) return(FALSE)
  household_type <- ifelse(qi %in% c(1,4),"single","couple")
  match_row <- fuel_allowance_eligibility[
    age >= fuel_allowance_eligibility$age_lower &
      age <= fuel_allowance_eligibility$age_upper &
      household_type == fuel_allowance_eligibility$household, ]

  if (nrow(match_row) == 0) {
    stop("No matching threshold found for given age and household type.")
  }

  return(income <= match_row$income_threshold)
}

#what fraction of our survey are eligible for fuel allowance
#hp_surv %>% mutate(fuel_allowance = is_eligible_fuel_allowance(actualage,income, ifelse(qi %in% c(1,4),"single","couple")))


#' retrofit_cost_model_esri
#'
#' @param ber_old old ber kWh/m2
#' @param ber_new new ber kWh/m2
#' @param house_type seai house type
#' @param region region
#' @param floor_area total floor area m2
#'
#' @returns cost in euros
#' @export
#'
#' @examples
retrofit_cost_model_esri <- function(ber_old,ber_new, house_type,region="Munster",floor_area=100){
  #Kren et al use is_Dublin,is_apartment, floor_area and houe type as controls
  #area dependence nstorys*4*sqrt(ground_floor_area)
  #retrofit_cost_model_esri(175,120,"semi_detached","Munster",120)

  if(ber_old <= ber_new) return(0)
  a_0 <- 29488
  a_1 <- -28.42
  a_2 <- 0.176

  cost <- a_0 + a_1*(ber_new-ber_old) + a_2*(ber_new-ber_old)^2
  #a_0 and a_1 depend on is_apartment
  cost*floor_area/100*ifelse(house_type=="apartment",0.75,1)*ifelse(region=="Dublin",1.25,1) %>% return()
}

#' retrofit_cost_marginal_model
#'
#' retrofit fit cost model based on marginal cost function k/ber^$alpha$. Default fit is based on cost matrix in ber_upgrade_cost_matrix
#'
#' @param ber_old old ber kWh/m2
#' @param ber_new new ber kWh/m2
#' @param house_type seai house type
#' @param qc2 region
#' @param floor_area total floor area in m2
#' @param params current parameter values
#'
#' @returns cost in euros
#' @export
#'
#' @examples
retrofit_cost_model_marginal <- function(ber_old,ber_new,house_type,qc2="Dublin",floor_area=100,params){
  #retrofit_cost_model_marginal(175,120,"semi_detached","Munster",100,params)
  alpha <- params$ber_upgrade_marginal_cost_alpha
  alpha <- 0.667
  k_100 <- params$ber_upgrade_marginal_cost_k
  k <- 69.8
  cost <- ifelse(ber_old > ber_new,k/(1-alpha)*(ber_old^(1-alpha)-ber_new^(1-alpha)),0)
  cost*floor_area/100*ifelse(qc2=="Dublin",1.25,1)*ifelse(house_type=="apartment",0.75,1)*100

}

#' gen_upgrade_cost_matrix
#'
#' generates an implied BER upgrade for given cost model params and house_type, region and floor area
#'
#' @param house_type house or apartment
#' @param region region
#' @param floor_area total floor area in m2
#' @param params parameters
#' @param model current choice "marginal" or "esri"
#' @param include_grant TRUE/FALSE
#'
#' @returns matrix
#' @export
#'
#' @examples
gen_upgrade_cost_matrix <- function(house_type,region="Dublin",floor_area=100,params,model="marginal",include_grant=FALSE){
  #gen_upgrade_cost_matrix("semi_detached","Dublin",100,params,"marginal")
  stopifnot(house_type %in% c("detached","semi_detached","apartment","terraced"))
  stopifnot(model %in% c("marginal","esri"))
  df <- tidyr::expand_grid(ber_old=seq(600,5,by=-5), ber_new=seq(600,5,by=-5))
  if(model=="marginal") df <- df %>% dplyr::rowwise() %>% dplyr::mutate(cost = retrofit_cost_model_marginal(ber_old,ber_new,house_type,region,floor_area,params))
  if(model=="esri") df <- df %>% dplyr::rowwise() %>% dplyr::mutate(cost = retrofit_cost_model_esri(ber_old,ber_new,house_type,region,floor_area))

  df <- df %>% dplyr::mutate(old_ber_score = get_ber_score(ber_old),new_ber_score=get_ber_score(ber_new))
  df <- df %>% dplyr::group_by(old_ber_score,new_ber_score) %>% dplyr::summarise(cost=mean(cost))
  df <- df %>% tidyr::pivot_wider(id_cols="old_ber_score",names_from="new_ber_score",values_from=cost)
  colnames <- df$old_ber_score
  df <- df %>% dplyr::ungroup() %>%  dplyr::select(-old_ber_score)
  colnames(df) <- colnames
  df <- tibble::tibble(ber_old = colnames) %>% dplyr::bind_cols(df)
  return(df)

}

#' heating_upgrade_tensor
#'
#' calculates an element of the heating system upgrade cost tensor (ber_old,tech_old) -> (ber_new,tech_new)
#'
#' @param ber_old old ber double
#' @param ber_new new ber double
#' @param tech_old old tech
#' @param tech_new new tech
#' @param house_type house type in seai codes
#' @param construction_year year,integer
#' @param region region
#' @param floor_area floor area of property in m2
#' @param params params
#' @param include_grants TRUE/FALSE
#'
#' @returns euros
#' @export
#'
#' @examples
heating_upgrade_tensor <- function(ber_old,ber_new,tech_old,tech_new,house_type,construction_year,region,floor_area,params,include_grants=TRUE){
  #heating_upgrade_tensor(175,120,tech_old="gas",tech_new = "heat_pump","detached",2003,"Dublin",100,params,include_grants=T)
  stopifnot(tech_old %in% c("electricity","heat_pump","solid_fuel","gas","oil"))
  stopifnot(tech_new %in% c("electricity","heat_pump","solid_fuel","gas","oil"))
  stopifnot(house_type %in% c("apartment","terraced","semi_detached","detached"))

  #annualised cost of new system vs annualised cost of old system
  #old system cost is opex only, discounts earlier investment to zero i.e.
  #we should allow early retirnment option, modelled as an additional cost.
  installation_type <- ifelse(tech_old == tech_new,"swap","new")
  #old_annualised_cost <- heating_system_operating_cost(tech_old,ber_old*floor_area,params)
  #assume "status quo" capex cost
  old_annualised_cost <- annualised_heating_system_cost(tech_old,installation="swap",ber_old*floor_area,house_type,construction_year,params)
  #ber upgrade cost including ber grants but excluding heat pump grants
  ber_upgrade_cost <- ifelse(ber_old==ber_new,0,retrofit_cost_model_marginal(ber_old,ber_new,house_type,region,floor_area,params)-
                               ifelse(include_grants,get_grant(ber_old,ber_new,construction_year,region,house_type,floor_area,is_fuel_allowance = F,is_heat_pump = F,params)$grant_value,0)) #
  #for now assume there is only one discount rate
  #infinite lifetime
  ber_upgrade_annualised_cost <- ber_upgrade_cost*params$delta. #
  new_annualised_cost <- annualised_heating_system_cost(tech_new,installation = installation_type,ber_new*floor_area,house_type,construction_year,params,include_grant=include_grants)
  new_annualised_cost <- new_annualised_cost + ber_upgrade_annualised_cost
  c("old"=old_annualised_cost, "new"=new_annualised_cost, "loss_or_gain"=new_annualised_cost-old_annualised_cost)
  #
}
#

#' optimise_upgrade
#'
#' find the financially optimum household ber upgrade
#'
#' @param ber_old old ber kWh/m2/year
#' @param tech_old new ber kWh/m2/year
#' @param house_type seai house type
#' @param construction_year integer
#' @param region region
#' @param floor_area total floor area (m2)
#' @param params current parameter values
#'
#' @returns
#' @export
#'
#' @examples
optimise_upgrade <- function(ber_old,tech_old,house_type,construction_year,region,floor_area,params){
  #optimise_upgrade(ber_old=175,tech_old = "oil",house_type="detached",2003,region="Munster",floor_area=100,params)
  df <- tibble::tibble()

  for(tech_new in c("heat_pump","oil","gas","electricity","solid_fuel")){

  fun <- function(ber_new){

    heating_upgrade_tensor(ber_old,ber_new,tech_old,tech_new, house_type,construction_year,region,floor_area,params)[[2]]

  }

   result <- optim(par=ber_old,fn=fun,lower=1,upper=ber_old,method="Brent")
   df <- df %>% dplyr::bind_rows(tibble::tibble(tech_new=tech_new, annualised_cost = result$value, ber_optimal=result$par))
   df$annualised_cost_old = annualised_heating_system_cost(tech_old,installation="swap",ber_old*floor_area,house_type,construction_year,params)
  }

  df <- df %>% dplyr::mutate(bill_savings = 100*(annualised_cost-annualised_cost_old)/annualised_cost_old)
  df %>% return()
}


#' grant_eligibility
#'
#' utility to determine which home energy efficiency grant scheme a homeowner is eligible for
#'
#' @param ber_old old ber (double)
#' @param ber_new new ber (double)
#' @param construction_year construction year(integer)
#' @param is_fuel_allowance fuel allowance qualification from  (boolean)
#'
#' @returns scheme
#' @export
#'
#' @examples grant_eligibility(175,20,2015,is_fuel_allowance=FALSE)
#'
grant_eligibility <- function(ber_old, ber_new, construction_year, is_fuel_allowance = FALSE) {
  # Early return for fuel allowance eligibility
  if (is_fuel_allowance & construction_year < 2006)  return("WarmerHomes")

  # For non-fuel allowance cases, check construction year first
  if (construction_year >= 2011) return("None")

  # Check OSS eligibility conditions once
  oss_eligible <- ber_new < 125 & ber_old >= 125 & ber_old - ber_new >= 100

  return(ifelse(oss_eligible,"OSS", "BetterEnergyHomes"))
}

#' get_grant
#'
#' find the grant amount available from SEAI.
#'
#' @param ber_old old ber, double
#' @param ber_new new ber, double
#' @param construction_year year of construction, integer
#' @param region region
#' @param house_type seai house type (related to q1)
#' @param floor_area m2
#' @param is_fuel_allowance TRUE/FALSE
#' @param is_heat_pump whether a heat pump is being installed TRUE/FALSE
#' @param params parameters
#'
#' @returns list(scheme, grant)
#' @export
#'
#' @examples  get_grant(ber_old=175,ber_new=100,construction_year = 2003,"Dublin","detached",100, is_fuel_allowance=FALSE,is_heat_pump=TRUE,scenario_params(sD,2025))
get_grant <- function(ber_old,ber_new,construction_year,region,house_type = c("semi_detached", "detached", "apartment", "mid-terrace", "end-terrace"),floor_area = 100,is_fuel_allowance = FALSE,is_heat_pump = FALSE,params) {

  # Input validation with more informative messages
  house_type <- match.arg(house_type)

  scheme0 <- grant_eligibility(ber_old,ber_new,construction_year,is_fuel_allowance)
  #print(scheme0)
  if(scheme0=="None" & !is_heat_pump) return(0)
  cost_estimate <- retrofit_cost_model_marginal(ber_old,ber_new,house_type,region,floor_area,params)
  #print(cost_estimate)
  if(scheme0=="WarmerHomes") return(cost_estimate)
  #seai_grants <- readr::read_csv("C:/Users/Joe/pkgs/hpmicrosimr/inst/extdata/seai_grants.csv",show_col_types = FALSE)
  relevant_grants <- seai_grants %>% dplyr::filter(scheme==scheme0,measure != "internal_wall_insulation",building_type==house_type)
  if(!is_heat_pump) relevant_grants <-  relevant_grants %>% dplyr::filter(!stringr::str_detect(measure,"heat_pump"))
  #print(relevant_grants)
  #print(dim(relevant_grants))
  max_grant <- pmin(cost_estimate,sum(relevant_grants$grant))*0.75  #assume 80% of grant measures are applicable
  #print(max_grant/cost_estimate)
  return(list(scheme=scheme0,grant_value=max_grant,cost_estimate=cost_estimate)) #factor of 0.8 because not all measures will apply
}
# SEAI Individual Grant Amounts (excluding Solar PV)
