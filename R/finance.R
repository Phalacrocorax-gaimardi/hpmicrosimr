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
#' The installation time is params$yeartime.
#'
#' The current version of this function assumed that ancilliary costs are halved if the technology is unchanged - "swap" vs "new".
#'
#'
#' @param tech primary heating technology
#' @param kW system capacity
#' @param installation_type "new" or "swap"
#' @param house_type for grant
#' @param construction_year for grant eligibility
#' @param grant_type choose "None" to exclude grant
#' @param params scenario params
#' @param include_vat TRUE/FALSE
#'
#' @returns euro cost
#' @export
#'
#' @examples
#' params <- scenario_params(sD,2026)
#' heating_system_capital_cost("heat_pump",18,installation_type="new","detached",2003,"BetterEnergyHomes",params)
#' #
#' heating_system_capital_cost("heat_pump",18,installation_type="new","detached",2003,"None",params)
#'
#' heating_system_capital_cost("heat_pump",18,installation_type="new","detached",2003,"WarmerHomes",scenario_params(sD,2026),include_vat = TRUE)
#' #
#' heating_system_capital_cost("heat_pump",18,installation_type="new","detached",2003,"OSS",scenario_params(sD,2026),include_vat = TRUE)
#'
#' heating_system_capital_cost("heat_pump",18,installation_type="new","detached",2003,"BetterEnergyHomes",scenario_params(sD,2019),include_vat = TRUE)

heating_system_capital_cost <- function(tech,kW,installation_type="new",house_type,construction_year,grant_type,params,include_vat = TRUE){
  #heating_system_capital_cost("heat_pump",18,"semi_detached","swap",params,include_grant=T)
  stopifnot(tech %in% c("heat_pump","gas","oil","electricity","solid_fuel"))
  stopifnot(house_type %in% c("detached","semi_detached","terraced","apartment"))
  stopifnot(grant_type %in% c("None","OSS","BetterEnergyHomes","WarmerHomes"))

  cost_service <- (tech_params[[paste(tech,"fixed_hours",installation_type,sep="_")]] + tech_params[[paste(tech,"kw_hours",installation_type,sep="_")]]*kW)*params$labour_cost
  cost_goods <- tech_params[[paste(tech,"fixed_tech_cost",installation_type,sep="_")]] + tech_params[[paste(tech,"fixed_ancilliary_cost",installation_type,sep="_")]]
  cost_goods <- cost_goods + kW*(tech_params[[paste(tech,"kw_ancilliary_cost",installation_type,sep="_")]] + tech_params[[paste(tech,"kw_tech_cost",installation_type,sep="_")]])
  #cost include ancilliary costs of not replacing an existing system
  capex_before_grant <- cost_goods+cost_service
  if(tech !="heat_pump")
   {return(capex_before_grant)}
  else
    {hp_grant <- heat_pump_grant(installation_type,house_type,construction_year,grant_type,params)
    #if hp_grant is "cost" then set hp_grant to actual cost
    hp_grant <- ifelse(hp_grant=="cost",capex_before_grant,hp_grant)
  return(capex_before_grant-hp_grant)}
}

#' annualised_heating_system_cost
#'
#' Current equivalent annualised cost of a home heating system installed at installation_time.
#'
#' this function uses a capital reduction factor with with a technology specific
#' Weibull survival function discount rate set from calibration (params$delta.).
#'
#' The annualised capex falls when the system age exceeds the expected lifetime. This corresponds
#' to the intuition that older systems that continue to operate are "free".
#'
#' @param tech heating technology
#' @param installation_time time of installation - different from params$yeartime
#' @param kW system capacity
#' @param installation_type "new" or "swap"
#' @param house_type seai house type
#' @param construction_year year, integer
#' @param grant_type grant type
#' @param params time of interest or current time
#'
#' @returns euro amount
#' @export
#'
#' @examples
#'  sapply(2010:2040, function(y) annualised_capex("heat_pump",18,2010,"swap","detached",2003,"WarmerHomes",scenario_params(sD,y)))
#' params <- scenario_params(sD,2026)
#' annualised_capex("heat_pump",18,params$yeartime,"new","detached",2003,"None",params)
#' annualised_capex("heat_pump",18,params$yeartime,"new","detached",2003,"BetterEnergyHomes",params)


annualised_capex <- function(tech,kW,installation_time, installation_type,house_type,construction_year,grant_type,params){
  #annualised_capex("gas",24,"semi_detached","swap",params,include_grant=TRUE)
  #if system exceeds it's expected lifetime set it's annualised capex to zero
  if(params$yeartime < installation_time) stop("yeartime must be later than installation time")
  beta <- tech_params[[paste(tech, "system_beta", sep = "_")]]
  lifetime <- tech_params[[paste(tech, "system_lifetime", sep = "_")]]
  #expected_lifetime_remaining <- expected_remaining_lifetime(tech,yeartime-installation_time)
  #if(params$yeartime - installation_time > expected_lifetime) return(0)
  capex <- heating_system_capital_cost(tech,kW,installation_type,house_type,construction_year,grant_type,scenario_params(sD,installation_time),include_vat = TRUE)
  #
  #print(capex)
  #continuous time discouint rate
  r <- log(1+params$delta.)
  annualised_capex <- capex*eac_weibull(lifetime,beta,params$yeartime - installation_time,r)
  return(annualised_capex)
}

#df <- tibble::tibble()
#for(y in 2010:2090){

#  params$yeartime <- y
#  df <- df %>% dplyr::bind_rows(tibble::tibble(year=y,a_cap=annualised_capex("oil",18,2010,"new","detached",2010,"None",params)))
#}


#tech_fuel_dictionary <- c("electricity"="electricity","heat_pump"= "electricity","solid_fuel"="solid_fuel","gas"="gas","oil"="oil")
#house_type_dictionary <- c("Flat or apartment"="apartment","Terraced house"="terraced","Semi-detached house"="semi_detached","Detached house"="detached","Other"="detached")

#' heating_system_operating cost
#'
#' Annual operational cost based on current fuel price, annual fuel consumption inferred from kWh heating_requirement,
#' and heating system efficiency based on installation year.
#'
#' Also includes an annual maintenance cost.
#'
#' @param tech heating technology
#' @param installation_time heating installation year
#' @param ber annual heating requirement in kWh
#' @param floor_area floor area in m2
#' @param params current parameters
#' @param include_rebound defaults to TRUE
#'
#' @returns
#' @export
#'
#' @examples
#' params <- scenario_params(sD,2026)
#' heating_system_operating_cost("oil",2003,200,100,params)
#'
#' heating_system_operating_cost("oil",2003,200,100,params,include_rebound=FALSE)
#'
#'
heating_system_operating_cost <- function(tech,installation_time,ber,floor_area,params,include_rebound=T){
  #heating_system_operating_cost("electricity",150*125,params)
  tech_fuel_dictionary <- c("electricity"="electricity","heat_pump"= "electricity","solid_fuel"="solid_fuel","gas"="gas","oil"="oil")
  fuel_type <- tech_fuel_dictionary[tech]
  efficiency <- heating_system_efficiency(tech,installation_time)
  heating_req <- ifelse(include_rebound, heating_requirement(ber,floor_area,params$r.,params),heating_requirement(ber,floor_area,1,params))
  opex <- heating_req*params[[paste(fuel_type,"price",sep="_")]]/(100*efficiency) #prices given in cents per kWh
  #electric heating may be primarily used at night. The night usage fraction is a fixed parameter at the moment
  if(tech %in% c("heat_pump","electricity")) opex <- opex*(params$night_rate_usage_factor*(1-params$night_rate_discount) + (1-params$night_rate_usage_factor))
  opex + params[[paste(tech,"system_maintenance",sep="_")]]
}

#' annualised_heating_system_cost
#'
#' annualised heating cost equal to the sum of annualised capital cost and operating cost
#'
#'
#' @param tech heating technology
#' @param installation_time time heating system installed
#' @param installation_type new or swap
#' @param ber energy rating
#' @param floor_area TFA
#' @param house_type housing type (q1)
#' @param construction_year year, integer
#' @param grant_type one of "OSS","BetterEnergyHomes", "None"
#' @param params parameter values at yeartime
#' @param include_rebound default FALSE
#'
#' @returns
#' @export
#'
#' @examples
#' params <- scenario_params(sD,2025)
#' # theoretical cost without rebound
#' annualised_heating_system_cost("heat_pump",2010,"new",200,100,"semi_detached",1997,grant_type="None",scenario_params(sD,2025))
#' # actual cost with rebound
#' annualised_heating_system_cost("heat_pump",2010,"new",200,100,"semi_detached",1997,grant_type="BetterEnergyHomes",scenario_params(sD,2025))
#'
annualised_heating_system_cost <- function(tech,installation_time,installation_type="swap",ber,floor_area,house_type,construction_year,grant_type,params,include_rebound=FALSE){
   #annualised_heating_system_cost("heat_pump","new",125*150,"semi_detached",2003,params,include_grant=T)
   stopifnot(tech %in% c("solid_fuel","gas","oil","heat_pump","electricity"))
   stopifnot(installation_type %in% c("swap","new"))
   stopifnot(grant_type %in% c("None","OSS","BetterEnergyHomes","WarmerHomes"))
   #specify heating system size
   heating_req <- heating_requirement(ber,floor_area,rebound=1,params) #full heating requirement
   kW <- heating_system_size(heating_req)
   #house_type <- ifelse(q1=="Flat or apartment","apartment","house")
   capex <- annualised_capex(tech,kW,installation_time,installation_type,house_type,construction_year,grant_type,params)
   opex <- heating_system_operating_cost(tech,installation_time,ber,floor_area,params,include_rebound)
   capex + opex

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
#' A retrofit fit cost model based a power law dependence of marginal upgrade cost on BER
#' on marginal cost function
#' \deqn{\frac{k}{BER^\alpha}}
#' so that the cost of upgrade from \eqn{BER_{old}} to \eqn{BER_{new}} is
#' \deqn{\frac{k}{1-\alpha}\left(BER_{old}^{1-\alpha}-BER_{new}^{1-\alpha}\right)}
#' The parameters \eqn{k} and \eqn{\alpha} are set in \emph{params}
#'
#' Default fit is based on cost matrix in ber_upgrade_cost_matrix. Assumes a square root dependence of cost on floor area (economy of scale)
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
#' @examples  retrofit_cost_model_marginal(200,100,"semi_detached","Dublin",100,scenario_params(sD,2026))
retrofit_cost_model_marginal <- function(ber_old,ber_new,house_type,qc2="Dublin",floor_area=100,params){
  #retrofit_cost_model_marginal(175,120,"semi_detached","Munster",100,params)
  alpha <- params$ber_upgrade_marginal_cost_alpha
  k_100 <- params$ber_upgrade_marginal_cost_k
  cost <- ifelse(ber_old > ber_new,k_100/(1-alpha)*(ber_old^(1-alpha)-ber_new^(1-alpha)),0)
  cost*sqrt(floor_area/100)*ifelse(qc2=="Dublin",1.25,1)*ifelse(house_type=="apartment",0.75,1)*100

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
#'
#' #gen_upgrade_cost_matrix("semi_detached","Dublin",100,params,model="marginal",FALSE)
#'
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
#' optionally, the current (old) heating system can be retained
#'
#' the effects of all grants can be excluded
#'
#' used with optimise_upgrade to determine the optimal upgrade path
#'
#' @param ber_old old ber double
#' @param ber_new new ber double
#' @param tech_old old tech
#' @param installation_time time of installation of current system (for estimate of old operating cost only)
#' @param tech_new new tech
#' @param house_type house type in seai codes
#' @param construction_year year,integer
#' @param region region
#' @param floor_area floor area of property in m2
#' @param params params
#' @param upgrade_heat TRUE if current heating system is upgraded
#' @param is_fuel_allowance TRUE/FALSE
#' @param include_grants TRUE/FALSE
#'
#' @returns list
#' @export
#'
#' @examples
#'
#' params <- scenario_params(sD,2016)
#' heating_upgrade_tensor(200,50,"oil",2003,"heat_pump","semi_detached",2005,"Dublin",120,params,upgrade_heat=TRUE,is_fuel_allowance=TRUE,include_grants=FALSE)
#'
#' heating_upgrade_tensor(200,100,"oil",2003,"heat_pump","semi_detached",1990,"Dublin",120,params,TRUE,TRUE,FALSE)
#'
#' heating_upgrade_tensor(200,200,"gas",2015,"heat_pump","semi_detached",1990,"Dublin",120,params,upgrade_heat = TRUE,is_fuel_allowance=FALSE,include_grants=FALSE)
#' heating_upgrade_tensor(200,200,"gas",2015,"heat_pump","semi_detached",1990,"Dublin",120,params,upgrade_heat = TRUE,is_fuel_allowance=FALSE,include_grants=TRUE)
#' heating_upgrade_tensor(200,100,"gas",2015,"gas","semi_detached",1990,"Dublin",120,params,FALSE,TRUE,TRUE)
#'
#' heating_upgrade_tensor(200,100,"gas",2015,"gas","semi_detached",1990,"Dublin",120,params,TRUE,TRUE,TRUE)
#'
#'
heating_upgrade_tensor <- function(ber_old,ber_new,tech_old,installation_time,tech_new,house_type,construction_year,region,floor_area,params,upgrade_heat=FALSE,include_grants=TRUE,is_fuel_allowance=FALSE){
  #heating_upgrade_tensor(175,120,tech_old="gas",tech_new = "heat_pump","detached",2003,"Dublin",100,params,include_grants=T)
  stopifnot(tech_old %in% c("electricity","heat_pump","solid_fuel","gas","oil"))
  stopifnot(tech_new %in% c("electricity","heat_pump","solid_fuel","gas","oil"))
  stopifnot(house_type %in% c("apartment","terraced","semi_detached","detached"))

  installation_type <- ifelse(tech_old == tech_new,"swap","new")
  #old_annualised_cost <- heating_system_operating_cost(tech_old,ber_old*floor_area,params)
  ifelse(include_grants, grant_type <- grant_eligibility(ber_old,ber_new,construction_year,is_fuel_allowance,params), grant_type <- "None")
  old_annualised_cost <- annualised_heating_system_cost(tech_old,installation_time,installation_type="swap",ber_old,floor_area,house_type,construction_year,grant_type="None",params)
  #ber upgrade cost including ber grants but excluding heat pump grants
  upgrade_grants <- efficiency_grant(ber_old,ber_new,construction_year,region,house_type,floor_area,is_fuel_allowance,params)
  #print(upgrade_grants$scheme)
  ber_upgrade_cost <- ifelse(ber_old==ber_new,0,retrofit_cost_model_marginal(ber_old,ber_new,house_type,region,floor_area,params)-
                               ifelse(include_grants,upgrade_grants$grant_value,0)) #

  ber_upgrade_annualised_cost <- ber_upgrade_cost*params$delta.
  grant_type <- ifelse(include_grants,upgrade_grants$scheme,"None")
  #print(upgrade_grants$grant_value)
  if(upgrade_heat){
   heating_req <- heating_requirement(ber_new,floor_area,rebound=1,params)
   kW_new <- heating_system_size(heating_req)
   #heating system if house already has a heat pump, not eligible for a grant
   heat_grant_type <- if(tech_old=="heat_pump") {"None"} else {if(tech_new=="heat_pump") {grant_type} else {"None"}}
   heating_sys_cost <- heating_system_capital_cost(tech_new,kW_new,installation_type,house_type,construction_year,heat_grant_type,params,include_vat = TRUE)
   #for now assume there is only one discount rate
   #infinite lifetime
   #should the heating system be retained or upgraded at this time?
   new_heat_annualised_cost <- annualised_heating_system_cost(tech_new,params$yeartime,installation_type,ber_new,floor_area,house_type,construction_year,heat_grant_type,params)
   new_annualised_cost <- new_heat_annualised_cost + ber_upgrade_annualised_cost
  #if the new tech is a heatpump consider the BetterEnergyHomes grant
  #if(tech_new=="heat_pump" & upgrade_grants$scheme=="None") upgrade_grants$scheme <- "BetterEnergyHomes"
  hp_grant <- ifelse(tech_new=="heat_pump" & include_grants,heat_pump_grant(installation_type,house_type,construction_year,upgrade_grants$scheme,params),0)
  if(hp_grant=="cost") hp_grant <-  heating_sys_cost
  #print(hp_grant)
  #c("old"=old_annualised_cost, "new"=new_annualised_cost, "loss_or_gain"=new_annualised_cost-old_annualised_cost)
  res <- list("new_cost"=as.numeric(new_annualised_cost),"old_cost"=as.numeric(old_annualised_cost),"savings"= (new_annualised_cost/old_annualised_cost-1), "upgrade_cost"=as.numeric(upgrade_grants$cost_estimate),"heating_sys_cost"=heating_sys_cost,"grant_type"=grant_type,"upgrade_grant"=as.numeric(upgrade_grants$grant_value),"heat_pump_grant"=as.numeric(hp_grant))
  res %>% tibble::as_tibble() %>% return()
  }
  else
  {
    #print("no heat upgrade")
    heating_req <- heating_requirement(ber_old,floor_area,rebound=1,params)
    kW_old <- heating_system_size(heating_req)
    #heating system if house already has a heat pump, not eligible for a grant
    params_old <- scenario_params(sD,installation_time)
    old_grant_type <- ifelse(include_grants & tech_old=="heat_pump",  grant_eligibility(ber_old,ber_new,construction_year,is_fuel_allowance,params_old),"None")
    heating_sys_cost <- heating_system_capital_cost(tech_old,kW_old,"new",house_type,construction_year,old_grant_type,params_old,include_vat = TRUE)
    #evluate heating system cost with old capital cost and current fuel system
    new_heat_annualised_cost <- annualised_heating_system_cost(tech_old,installation_time,"new",ber_new,floor_area,house_type,construction_year,old_grant_type,params)
    new_annualised_cost <- new_heat_annualised_cost + ber_upgrade_annualised_cost
    #if the new tech is a heatpump consider the BetterEnergyHomes grant
    #if(tech_new=="heat_pump" & upgrade_grants$scheme=="None") upgrade_grants$scheme <- "BetterEnergyHomes"
    hp_grant <- ifelse(tech_new=="heat_pump",heat_pump_grant(installation_type,house_type,construction_year,old_grant_type,params_old),0)
    if(hp_grant=="cost") hp_grant <-  heating_sys_cost
    #print(hp_grant)
    #c("old"=old_annualised_cost, "new"=new_annualised_cost, "loss_or_gain"=new_annualised_cost-old_annualised_cost)
    res <- list("new_cost"=as.numeric(new_annualised_cost),"old_cost"=as.numeric(old_annualised_cost),"savings"= (new_annualised_cost/old_annualised_cost-1), "upgrade_cost"=as.numeric(upgrade_grants$cost_estimate),"heating_sys_cost"=heating_sys_cost,"grant_type"=grant_type,"upgrade_grant"=as.numeric(upgrade_grants$grant_value),"heat_pump_grant"=as.numeric(hp_grant))
    res %>% tibble::as_tibble() %>% return()
  }
}
#

#' optimise_upgrade
#'
#' optimise_upgrade finds the financially optimum household energy upgrade
#'
#' The potential new heating technologies (tech_new) considered are current tech and heat pump for all grant types.
#'
#' The
#'
#' If fuel allowance is TRUE and the hosuehold qualifies for WarmerHomes then it is assumed that the property is upgraded to B2 (120 kWh/m2/y) standard
#'
#' All grant_types apart from WarmerHomes requires BER optimisation
#'
#' @param ber_old old ber kWh/m2/year
#' @param tech_old new ber kWh/m2/year
#' @param installation_time year of installation of old (current) tech
#' @param house_type seai house type
#' @param construction_year integer
#' @param region region
#' @param floor_area total floor area (m2)
#' @param params current parameter values
#' @param upgrade_heat if TRUE then upgrade the current heating system
#' @param is_fuel_allowance TRUE/FALSE
#' @param include_grants defaults to TRUE
#'
#' @returns a
#' @export
#'
#' @examples
#'
#' params <- scenario_params(sD,2026)
#'
#' optimise_upgrade(200,"gas",2010,"detached",2010,"Dublin",210,params,TRUE,FALSE,TRUE)
#'
#' optimise_upgrade(200,"oil",2010,"detached",2010,"Dublin",210,params,TRUE,FALSE,TRUE)
#'
#' optimise_upgrade(200,"heat_pump",2010,"detached",2010,"Dublin",210,params,TRUE,FALSE,TRUE)
#'
#' #WarmerHomes
#' optimise_upgrade(200,"gas",2010,"detached",2005,"Dublin",100,params,is_fuel_allowance=TRUE)
#'
#'

optimise_upgrade <- function(ber_old,tech_old,installation_time,house_type,construction_year,region,floor_area,params,upgrade_heat=TRUE,is_fuel_allowance,include_grants=TRUE){

  #First check whether qualifies for WarmerHomes
  grant_type <- ifelse(include_grants,grant_eligibility(ber_old,ber_old,construction_year,is_fuel_allowance,params), "None")
  #
  #print(grant_type)
  if(upgrade_heat){
  if(grant_type=="WarmerHomes"){
    #WarmerHomes is assume to retain current heating system
    #tech_new <- ifelse(params$yeartime >= 2025,"heat_pump",tech_old)
    #df <- tibble::tibble(tech_old=tech_old,tech_new=c(tech_old,"heat_pump"), ber_old=ber_old,ber_new=120)
    df <- tibble::tibble()
    for(tech_new in c(tech_old,"heat_pump")){
     df1 <- tibble::tibble(tech_old=tech_old,tech_new = tech_new,ber_old=ber_old,ber_new =120) %>% dplyr::bind_cols(heating_upgrade_tensor(ber_old,120,tech_old,installation_time,tech_new,house_type,construction_year,region,floor_area,params,upgrade_heat,is_fuel_allowance,include_grants=TRUE))
     df <- df %>% dplyr::bind_rows(df1)
    }
    #df <- df %>% dplyr::mutate(bill_savings = 100*(annualised_cost-annualised_cost_old)/annualised_cost_old)
    return(df)
  }
  else
  {
  df <- tibble::tibble()
  #for(tech_new in c("heat_pump","oil","gas","electricity","solid_fuel")){
  #NB assume that the choice is between retaining current tech or heat pump
  for(tech_new in c(tech_old,"heat_pump")){
  #print(tech_new)
  fun <- function(ber_new){

    heating_upgrade_tensor(ber_old,ber_new,tech_old,installation_time,tech_new, house_type,construction_year,region,floor_area,params,upgrade_heat,is_fuel_allowance=FALSE,include_grants)$new_cost

  }

   result <- optim(par=ber_old,fn=fun,lower=1,upper=ber_old,method="Brent")
   ber_optimal <- result$par
   df <- df %>% dplyr::bind_rows(tibble::tibble(tech_old=tech_old,tech_new=tech_new, ber_old=ber_old,ber_new=ber_optimal))
  }

  df <- df %>% dplyr::rowwise() %>% dplyr::mutate(result = list(heating_upgrade_tensor(ber_old,ber_new,tech_old,installation_time,
                                                                                       tech_new,house_type,construction_year,
                                                                                       region,floor_area,params,upgrade_heat,is_fuel_allowance=FALSE,include_grants))) %>% tidyr::unnest_wider(result)
 #df <- df %>% dplyr::mutate(bill_savings = 100*(annualised_cost-annualised_cost_old)/annualised_cost_old)
  df <- df %>% dplyr::filter(tech_new %in% c(tech_old,"heat_pump")) #%>% dplyr::slice_min(new_cost)
  }
  return(df %>% dplyr::distinct())
  }
  else{
    #First check whether qualifies for WarmerHomes
      if(grant_type=="WarmerHomes"){
        #WarmerHomes is assume to retain current heating system
        #tech_new <- ifelse(params$yeartime >= 2025,"heat_pump",tech_old)
        #df <- tibble::tibble(tech_old=tech_old,tech_new=c(tech_old,"heat_pump"), ber_old=ber_old,ber_new=120)
        df <- tibble::tibble()
        for(tech_new in c(tech_old,"heat_pump")){
          df1 <- tibble::tibble(tech_old=tech_old,tech_new = tech_old,ber_old=ber_old,ber_new =120) %>% dplyr::bind_cols(heating_upgrade_tensor(ber_old,120,tech_old,installation_time,tech_new,house_type,construction_year,region,floor_area,params,upgrade_heat,is_fuel_allowance=TRUE,include_grants))
          df <- df %>% dplyr::bind_rows(df1)
        }
        #df <- df %>% dplyr::mutate(bill_savings = 100*(annualised_cost-annualised_cost_old)/annualised_cost_old)
        return(df)
      }
      else
      {
        df <- tibble::tibble()
        #for(tech_new in c("heat_pump","oil","gas","electricity","solid_fuel")){
        #NB assume that the choice is between retaining current tech or heat pump

          fun <- function(ber_new){

            heating_upgrade_tensor(ber_old,ber_new,tech_old,installation_time,tech_old, house_type,construction_year,region,floor_area,params,upgrade_heat=TRUE,is_fuel_allowance=FALSE,include_grants)$new_cost

          }

          result <- optim(par=ber_old,fn=fun,lower=1,upper=ber_old,method="Brent")
          ber_optimal <- result$par
          df <- df %>% dplyr::bind_rows(tibble::tibble(tech_old=tech_old,tech_new=tech_old, ber_old=ber_old,ber_new=ber_optimal))
        }

        df <- df %>% dplyr::rowwise() %>% dplyr::mutate(result = list(heating_upgrade_tensor(ber_old,ber_new,tech_old,installation_time,
                                                                                             tech_old,house_type,construction_year,
                                                                                             region,floor_area,params,upgrade_heat,FALSE,include_grants))) %>% tidyr::unnest_wider(result)
        #df <- df %>% dplyr::mutate(bill_savings = 100*(annualised_cost-annualised_cost_old)/annualised_cost_old)
        #df <- df %>% dplyr::filter(tech_new %in% c(tech_old,"heat_pump")) #%>% dplyr::slice_min(new_cost)
      return(df %>% dplyr::distinct())

  }
}

#' optimise_heat
#'
#' Find the financially optimum household heating system replacement with no BER upgrade. The choice is between replacing current system or installing a heat pump.
#'
#' Ff current system is a heat pump, choose between a heat pump swap or reversion to gas.
#'
#' @param ber old ber kWh/m2/year
#' @param tech_old new ber kWh/m2/year
#' @param installation_time year of installation of old tech
#' @param house_type seai house type
#' @param construction_year integer
#' @param region region
#' @param floor_area total floor area (m2)
#' @param params current parameter values
#'
#' @returns data frame (2 or 1 rows)
#' @export
#'
#' @examples
#' params <- scenario_params(sD,2017)
#' optimise_heat(200,"oil",2000,"terraced",1980,"Munster",90,params)
#'
optimise_heat <- function(ber,tech_old,installation_time,house_type,construction_year,region,floor_area,params){
  #optimise_upgrade(ber_old=175,tech_old = "oil",house_type="detached",2003,region="Munster",floor_area=100,params)
      df0 <- tibble::tibble("tech_old"=tech_old,"tech_new"=c(tech_old,"heat_pump"))
      upgrade_fun <- function(tech_old, tech_new){
        heating_upgrade_tensor(ber,ber,tech_old,installation_time,tech_new,house_type,construction_year,region,floor_area,params)
      }

      df1 <- purrr::pmap(df0,upgrade_fun)
      df1 <- do.call(rbind,df1)

      df0 %>% dplyr::bind_cols(df1) %>% dplyr::distinct()
      #advantage of adopting
      #stick_cost <- df1 %>% dplyr::filter(tech_old==tech_new) %>% dplyr::pull(new_cost)
      #switch_cost <- df1 %>% dplyr::filter(tech_old!=tech_new) %>% dplyr::pull(new_cost)
      #savings <- switch_cost/stick_cost -1
}

#' heat_pump_savings
#'
#' find the financially optimum household heating system replacement. Choose between current system and installing a heat pump.
#'
#' if current system is a heat pump, choose between a heat pump swap or reversion to gas.
#'
#' @param ber old ber kWh/m2/year
#' @param tech_old new ber kWh/m2/year
#' @param installation_time year of installation of old tech
#' @param house_type seai house type
#' @param construction_year integer
#' @param region region
#' @param floor_area total floor area (m2)
#' @param params current parameter values
#'
#' @returns one row dataframe
#' @export
#'
#' @examples
#' heat_pump_savings(120,"gas",2010,"detached",1995,"Munster",120,params=scenario_params(sD,2025))

heat_pump_savings <- function(ber,tech_old,installation_time,house_type,construction_year,region,floor_area,params){

  df <- optimise_heat(ber,tech_old,installation_time,house_type,construction_year,region,floor_area,params)
  stick_cost <- df %>% dplyr::filter(tech_old==tech_new) %>% dplyr::pull(new_cost)
  switch_cost <- ifelse(tech_old=="heat_pump",NA,df %>% dplyr::filter(tech_old!=tech_new) %>% dplyr::pull(new_cost))
  savings <- ifelse(tech_old=="heat_pump",NA, switch_cost/stick_cost -1)
  grant <- df %>% dplyr::filter(tech_new=="heat_pump") %>% dplyr::pull(heat_pump_grant)
  tibble::tibble(stick_cost=stick_cost, switch_cost=switch_cost,savings=savings, heat_pump_grant = grant) %>% return()
}


#' heat_pump_upgrade_savings
#'
#' savings (equivalent annual cost % savings) by replacing current home heating system with a heat pump
#'
#' excludes WarmerHomes by assumoing is_fuel_allowance=F
#'
#' diagnostic function not used by other functions
#'
#' @param ber_old old ber, assumed to be the same as ber_new (no energy efficiency upgrade)
#' @param tech_old old tech
#' @param installation_time time of installation of old tech
#' @param house_type seai house type
#' @param construction_year house construction year
#' @param region region
#' @param floor_area floor area in m2
#' @param params scenario params at current yeartime
#' @param include_grants TRUE or FALSE
#'
#' @returns savings
#' @export
#'
#' @examples
#' params <- scenario_params(sD,2026)
#' heat_pump_upgrade_savings(200,"gas",2000,"detached",1990,"Munster",120,params,include_grants=TRUE)
#'
#'
heat_pump_upgrade_savings <-  function(ber_old,tech_old,installation_time,house_type,construction_year,region,floor_area,params,include_grants){
  #
  df <- optimise_upgrade(ber_old,tech_old,installation_time,house_type,construction_year,region,floor_area,params,upgrade_heat=TRUE,is_fuel_allowance=FALSE,include_grants)
  stick_cost <- df %>% dplyr::filter(tech_old==tech_new) %>% dplyr::pull(new_cost)
  switch_cost <- ifelse(tech_old=="heat_pump",NA,df %>% dplyr::filter(tech_old!=tech_new) %>% dplyr::pull(new_cost))
  #stick_ber <- df %>% dplyr::filter(tech_old==tech_new) %>% dplyr::pull(ber_new)
  #switch_ber <- ifelse(tech_old=="heat_pump",NA,df %>% dplyr::filter(tech_old!=tech_new) %>% dplyr::pull(ber_new))
  savings <- ifelse(tech_old=="heat_pump",NA, switch_cost/stick_cost -1)
  grant <- df %>% dplyr::filter(tech_new=="heat_pump") %>% dplyr::pull(heat_pump_grant)
  tibble::tibble(stick_cost=stick_cost, switch_cost=switch_cost,savings=savings, heat_pump_grant = grant) %>% return()

}


#' grant_eligibility
#'
#' utility to determine which home energy efficiency grant scheme a homeowner is or was eligible
#'
#' @param ber_old old ber (double)
#' @param ber_new new ber (double)
#' @param construction_year construction year(integer)
#' @param is_fuel_allowance fuel allowance qualification from  (boolean)
#' @param params used for yeartime to determine grant availability
#'
#' @returns scheme one of "None", "WarmerHomes", "OSS", "BetterEner
#' @export
#'
#' @examples
#'
#' grant_eligibility(175,20,2005,is_fuel_allowance=FALSE,scenario_params(sD,2026))
#' #no grants before 2015
#' grant_eligibility(175,20,1990,is_fuel_allowance=TRUE,scenario_params(sD,2005))
#' #warmer homes
#' grant_eligibility(175,20,2005,is_fuel_allowance=TRUE,scenario_params(sD,2026))
#' #newer house
#' grant_eligibility(175,175,2000,is_fuel_allowance=TRUE,scenario_params(sD,2026))
#'
grant_eligibility <- function(ber_old, ber_new, construction_year, is_fuel_allowance = FALSE,params) {
  # Early return for fuel allowance eligibility
  #fuel allowance must be C or worse and new ber must eb at least b2
  #elsewhere it is assume that WarmerHomes grants
  #Warmer Homes requires C or worse
  if (is_fuel_allowance & construction_year < 2006 & ber_old >= 150 & params$yeartime > params$warmer_homes_introduction)  return("WarmerHomes")

  # For non-fuel allowance cases, check construction year first
  if (construction_year >= 2011 | params$yeartime < params$better_energy_introduction) return("None")

  # Check OSS eligibility conditions once
  oss_eligible <- ber_new < 125 & ber_old >= 125 & ber_old - ber_new >= 100 & params$yeartime > params$oss_introduction

  return(ifelse(oss_eligible,"OSS", "BetterEnergyHomes"))
}

#' efficiency_grant
#'
#' find the grant amount available from SEAI for energy efficiency upgrades. This function *excludes* grants for heat pumps.
#'
#' @param ber_old old ber, double
#' @param ber_new new ber, double
#' @param construction_year year of construction, integer
#' @param region region
#' @param house_type seai house type (related to q1)
#' @param floor_area m2
#' @param is_fuel_allowance TRUE/FALSE
#' @param params parameters
#' @param randomise whether to assign grant elements randomly or not. For testing set to FALSE
#'
#' @returns list(scheme, grant)
#' @export
#'
#' @examples
#'
#' params <- scenario_params(sD,2025)
#' efficiency_grant(200,105,2003,"Dublin","detached",100, FALSE,params,randomise=TRUE)
#'
efficiency_grant <- function(ber_old,ber_new,construction_year,region,house_type,floor_area = 100,is_fuel_allowance = FALSE,params,randomise=FALSE) {

  #Input validation with more informative messages
  stopifnot(house_type %in% c("semi_detached", "detached", "apartment", "terraced","apartment"))
  set.seed(as.integer(Sys.time()))
  cost_estimate <- retrofit_cost_model_marginal(ber_old,ber_new,house_type,region,floor_area,params)
  scheme0 <- grant_eligibility(ber_old,ber_new,construction_year,is_fuel_allowance,params)
  #(print(scheme0)
  if(scheme0=="None") return(list(scheme=scheme0,grant_value=0,cost_estimate=cost_estimate, grant_share=0))
  #print(cost_estimate)
  #in case of WarmerHomes cap grant at B2 (100)
  if(scheme0=="WarmerHomes") { value <- retrofit_cost_model_marginal(ber_old,pmax(ber_new,100),house_type,region,floor_area,params)
    return(list(scheme=scheme0,grant_value=value,cost_estimate=cost_estimate, grant_share=value/cost_estimate))
  }
  #data.table::setDT(seai_grants)
  #Fast filtering using data.table syntax
  #relevant_grants <- seai_grants[scheme == scheme0 & building_type == house_type]
  if(randomise) { relevant_grants <- seai_grants %>% dplyr::filter(scheme==scheme0,building_type==house_type)

  relevant_grants <-  relevant_grants %>% dplyr::filter(!stringr::str_detect(measure,"heat_pump"))
  #relevant_grants_mean <- relevant_grants %>% dplyr::mutate(measure=replace(measure, str_detect(measure,"attic|rafter"),"roof"))
  #relevant_grants_mean <- relevant_grants %>% dplyr::mutate(measure=replace(measure, str_detect(measure,"wall"),"wall"))

  #randomly select grant elements that refer duplicate work
  oss_rejected_elements <- c(sample(c("attic_insulation","rafter_insulation"),1),
                         sample(c("internal_wall_insulation","external_wall_insulation","cavity_wall_insulation"),2,replace = F),
                         sample(c("floor_insulation",""),1),sample(c("windows",""),1))
  beh_rejected_elements <- sample(c("internal_wall_insulation","external_wall_insulation","cavity_wall_insulation"),2)

  ifelse(scheme0=="OSS", relevant_grants <- relevant_grants %>% dplyr::filter(!(measure %in% oss_rejected_elements)),
         relevant_grants <- relevant_grants %>% dplyr::filter(!(measure %in% beh_rejected_elements)))

  #print(relevant_grants)
  #print(dim(relevant_grants))
  max_grant <- pmin(cost_estimate,sum(relevant_grants$grant))  #assume 75% of grant measures are applicable
  #print(max_grant/cost_estimate)
  return(list(scheme=scheme0,grant_value=max_grant,cost_estimate=cost_estimate, grant_share=max_grant/cost_estimate)) #factor of 0.8 because not all measures will apply
  }
  if(!randomise){
   relevant_grants <- seai_grants_average %>% dplyr::filter(scheme==scheme0,building_type==house_type)
   relevant_grants <-  relevant_grants %>% dplyr::filter(!stringr::str_detect(measure,"heat_pump"))
   #relevant_grants_mean <- relevant_grants %>% dplyr::mutate(measure=replace(measure, str_detect(measure,"attic|rafter"),"roof"))
   #relevant_grants_mean <- relevant_grants %>% dplyr::mutate(measure=replace(measure, str_detect(measure,"wall"),"wall"))
   max_grant <- pmin(cost_estimate,sum(relevant_grants$grant)-4000)  #manual adjustment
   #print(max_grant/cost_estimate)
   return(list(scheme=scheme0,grant_value=max_grant,cost_estimate=cost_estimate, grant_share=max_grant/cost_estimate)) #factor of 0.8 because not all measures will apply
  }
  }

#' heat_pump_grant
#'
#' heat pump grant, depending on grant_type and installation time (params$yeartime)
#'
#' for WarmerHomes, it is assumed that the grant covers heat pumps after 1 jan 2025 but not before.
#'
#' @param installation_type "swap" or "new"
#' @param house_type house type
#' @param construction_year year
#' @param grant_type one of "None","BetterEnergyHomes","OSS","WarmerHomes"
#' @param params current parameters
#'
#' @return grant amount in euros
#' @export
#'
#' @examples
#' #WarmerHomes
#' heat_pump_grant("new","apartment",2003,"WarmerHomes",scenario_params(sD,2025.5))
#'
#' heat_pump_grant("new","apartment",2003,"WarmerHomes",scenario_params(sD,2024))
#'
#' heat_pump_grant("new","detached",2003,"BetterEnergyHomes",scenario_params(sD,2024))

heat_pump_grant <- function(installation_type,house_type,construction_year,grant_type,params) {
  # Return grant amount based on date and type
  stopifnot(house_type %in% c("detached","semi_detached","terraced","apartment"))
  stopifnot(grant_type %in% c("None","BetterEnergyHomes","OSS","WarmerHomes"))
  if(params$yeartime < params$oss_introduction & grant_type=="OSS") stop("OSS does not exist yet, try BetterEnergyHomes")
  #if(grant_type=="OSS" & params$yeartime < params$hp_grant_increase) stop("OSS did not exist at this time")
  #for now assume no heat pump grants avaiable for WarmerHomes
  if(installation_type=="swap" | params$yeartime < params$hp_grant_introduction | params$yeartime > params$hp_grant_removal | construction_year > 2020 | grant_type %in% c("None")) return(0)
  # No grant before Q2 2018
  if(grant_type=="WarmerHomes" & params$yeartime < 2025) return(0)
  if(grant_type=="WarmerHomes" & params$yeartime >= 2025) return("cost")

  if(grant_type %in% c("BetterEnergyHomes","OSS") & params$yeartime < params$hp_grant_increase) {
    # Original grant scheme: flat rates, same for all dwelling types
    return(3500)
  }
  hp_grants <- seai_grants %>% dplyr::filter(building_type==house_type, stringr::str_detect(measure,"heat_pump"),scheme==grant_type)
  grant <- hp_grants$grant
  ifelse(length(grant)==1,return(grant), return(grant[1]+sample(c(1,0),1)*grant[2]+grant[3]))
}

