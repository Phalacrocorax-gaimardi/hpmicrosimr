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

#' heating_system_capital_cost
#'
#' cost of a new heating system. Includes VAT using the "two-thirds" rule.
#'
#' The installation time is params$yeartime.
#'
#' The current version of this function assumed that ancilliary costs are halved if the technology is unchanged - "swap" vs "new".
#'
#' @param tech primary heating technology
#' @param kW system capacity
#' @param installation_type "new" or "swap"
#' @param house_type for grant
#' @param construction_year for grant eligibility
#' @param grant_type choose "None" to exclude grant e.g. because HLI > threshold
#' @param params scenario params
#' @param include_vat TRUE/FALSE
#'
#' @returns a list showing actual cost, grant and cost after grant
#' @export
#'
#' @examples
#' params <- scenario_params(sD,2026)
#' heating_system_capital_cost("heat_pump",18,installation_type="new","detached",2003,"BetterEnergyHomes",params)
#' #
#' heating_system_capital_cost("heat_pump",18,installation_type="new","detached",2003,"BetterEnergyHomes",params)
#'
#' heating_system_capital_cost("heat_pump",18,installation_type="new","detached",2003,"WarmerHomes",scenario_params(sD,2026),include_vat = TRUE)
#' #
#' heating_system_capital_cost("heat_pump",5,installation_type="new","detached",2003,"OSS",scenario_params(sD,2026),include_vat = TRUE)
#'
#' heating_system_capital_cost("oil",8,installation_type="new","detached",2003,"OSS",params,include_vat = TRUE)
#' heating_system_capital_cost("heat_pump",5,installation_type="new","detached",2003,"None",params,include_vat = TRUE)

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
  if(tech !="heat_pump") {return(list("cost"=capex_before_grant,"grant"=0, "cost_after_grant"=capex_before_grant))}
  else
    {hp_maxgrant <- heat_pump_grant(installation_type,house_type,construction_year,grant_type,params)
    #if hp_grant is "cost" then set hp_grant to actual cost
    if (identical(hp_maxgrant, "cost")) hp_maxgrant <- capex_before_grant
  return(list("cost"=capex_before_grant,"grant"= pmin(capex_before_grant,hp_maxgrant),"cost_after_grant"=pmax(0,capex_before_grant-hp_maxgrant)))}
}

#heating_system_capital_cost <- Vectorize(heating_system_capital_cost, vectorize.args = c("tech","kW","installation_type","house_type","construction_year","grant_type"))


#' annualised_heating_system_cost
#'
#' Current equivalent annualised cost of a home heating system installed at installation_time.
#'
#' this function uses a capital reduction factor with with a technology specific
#' Weibull survival function. The annualised capex falls when the system age exceeds the expected lifetime. This corresponds
#' to the intuition that older systems that continue to operate are "free".

#'
#' The discount rate set from calibration (params$r.).
#'
#' A present bias (above a capex threshold) params$beta. is included.
#'
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
#' params$beta. <- 0.5
#' annualised_capex("heat_pump",18,params$yeartime,"new","detached",2003,"None",params)
#' annualised_capex("heat_pump",8,params$yeartime,"new","detached",2003,"BetterEnergyHomes",params)

annualised_capex <- function(tech,kW,installation_time, installation_type,house_type,construction_year,grant_type,params){
  #annualised_capex("gas",24,"semi_detached","swap",params,include_grant=TRUE)
  #if system exceeds it's expected lifetime set it's annualised capex to zero
  if(params$yeartime < installation_time) stop("yeartime must be later than installation time")
  beta <- tech_params[[paste(tech, "system_beta", sep = "_")]]
  lifetime <- tech_params[[paste(tech, "system_lifetime", sep = "_")]]
  #Exclude grant for old installations installation_time < params$yeartime exclude grant??
  #
  #ifelse(installation_time < params$yeartime,
  #capex <- heating_system_capital_cost(tech,kW,installation_type,house_type,construction_year,"None",scenario_params(sD,installation_time),include_vat = TRUE)$cost_after_grant,
  capex <- heating_system_capital_cost(tech,kW,installation_type,house_type,construction_year,grant_type,params,include_vat = TRUE)$cost_after_grant
  #)
  #
  #print(capex)
  #continuous time discount rate
  r <- log(1+params$r.) #annual to continuous time
  eac <- eac_weibull(lifetime,beta,params$yeartime - installation_time,r)
  effective_bias <- ifelse(grant_type=="OSS", params$beta.*params$eta., params$beta.) #includes "sludge-hassle" for OSS
  annualised_capex <- ifelse(capex <= params$present_bias_threshold, capex*eac,eac*(params$present_bias_threshold+ (capex-params$present_bias_threshold)/effective_bias))
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
#' @param hli heat loss indicator
#' @param tech heating technology
#' @param installation_time heating installation year
#' @param floor_area floor area in m2
#' @param params current parameters
#' @param include_rebound defaults to FALSE
#'
#' @returns
#' @export
#'
#' @examples
#' params <- scenario_params(sD,2026)
#' heating_system_operating_cost(2, "oil",2003,100,params)
#'
#' heating_system_operating_cost(3,"oil",2003,100,params,include_rebound=FALSE)
#'
#' heating_system_operating_cost(3,"oil",2003,100,params,include_rebound=TRUE)
#'
heating_system_operating_cost <- function(hli,tech,installation_time,floor_area,params,include_rebound=FALSE){
  #heating_system_operating_cost("electricity",150*125,params)
  tech_fuel_dictionary <- c("electricity"="electricity","heat_pump"= "electricity","solid_fuel"="solid_fuel","gas"="gas","oil"="oil")
  fuel_type <- tech_fuel_dictionary[tech]
  efficiency <- heating_system_efficiency(tech,installation_time)
  heating_req <- ifelse(include_rebound, space_heating_requirement(hli,floor_area,params$rho.,params),space_heating_requirement(hli,floor_area,0,params))
  opex <- heating_req*params[[paste(fuel_type,"price",sep="_")]]/(100*efficiency) #prices given in cents per kWh
  #electric heating may be primarily used at night. The night usage fraction is a fixed parameter at the moment
  if(tech %in% c("heat_pump","electricity")) opex <- opex*(params$night_rate_usage_factor*(1-params$night_rate_discount) + (1-params$night_rate_usage_factor))
  opex + params[[paste(tech,"system_maintenance",sep="_")]]
}

#' annualised_heating_system_cost
#'
#' The annualised heating cost or "equivalent annual cost" equal to the sum of annualised capital cost (calculated with Weibull survival) and operating cost.
#'
#' See annualised_capex for details of present bias assumptions
#'
#' @param hli heat loss indicator
#' @param tech heating technology
#' @param installation_time time heating system installed
#' @param installation_type new or swap
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
#' annualised_heating_system_cost(2.4,"heat_pump",2010,"new",100,"semi_detached",1997,grant_type="None",params)
#' # actual cost with rebound
#' annualised_heating_system_cost(2.4,"oil",2010,"new",100,"semi_detached",1997,grant_type="None",params)
#' params$beta. <- 0.5
#' annualised_heating_system_cost(2.4,"heat_pump",2010,"new",100,"semi_detached",1997,grant_type="None",params)
#' annualised_heating_system_cost(2.4,"oil",2010,"new",100,"semi_detached",1997,grant_type="BetterEnergyHomes",params)

annualised_heating_system_cost <- function(hli,tech,installation_time,installation_type="swap",floor_area,house_type,construction_year,grant_type,params,include_rebound=FALSE){
   #annualised_heating_system_cost("heat_pump","new",125*150,"semi_detached",2003,params,include_grant=T)
   stopifnot(tech %in% c("solid_fuel","gas","oil","heat_pump","electricity"))
   stopifnot(installation_type %in% c("swap","new"))
   stopifnot(grant_type %in% c("None","OSS","BetterEnergyHomes","WarmerHomes"))
   #specify heating system size
   heating_req <- space_heating_requirement(hli,floor_area,rebound=0,params) #full heating requirement with no rebound!
   kW <- peak_heating_demand(hli,floor_area)
   #house_type <- ifelse(q1=="Flat or apartment","apartment","house")
   capex <- annualised_capex(tech,kW,installation_time,installation_type,house_type,construction_year,grant_type,params)
   opex <- heating_system_operating_cost(hli,tech,installation_time,floor_area,params,include_rebound)
   capex + opex

}

#annualised_heating_system_cost <- Vectorize(annualised_heating_system_cost,vectorize.args=c("tech","installation_time","installation_type","ber","floor_area","house_type","construction_year","grant_type"))


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
#' ESRI quadratic fit to OSS retrofit data
#'
#' Material costs are fixed. ~30% of 2024 cost is assumed to reflect skill labour costs.
#'
#'
#' @param ber_old old ber kWh/m2
#' @param ber_new new ber kWh/m2
#' @param house_type seai house type
#' @param region region
#' @param floor_area total floor area m2
#' @param params sD
#'
#' @returns cost in euros
#' @export
#'
#'
#'
#' @examples
retrofit_cost_model_esri <- function(ber_old,ber_new, house_type,region="Munster",floor_area=100,params){
  #Kren et al use is_Dublin,is_apartment, floor_area and houe type as controls
  #area dependence nstorys*4*sqrt(ground_floor_area)
  #retrofit_cost_model_esri(175,120,"semi_detached","Munster",120)

  if(ber_old <= ber_new) return(0)
  a_0 <- 29488
  a_1 <- -28.42
  a_2 <- 0.176

  cost <- a_0 + a_1*(ber_new-ber_old) + a_2*(ber_new-ber_old)^2
  #a_0 and a_1 depend on is_apartment
  res_2024 <- cost*floor_area/100*ifelse(house_type=="apartment",0.75,1)*ifelse(region=="Dublin",1.25,1)
  #adjust by 2024 labour cost
  res_2024*((1-params$ber_upgrade_labour_cost_share) + params$ber_upgrade_labour_cost_share*params$labour_cost/60) %>% return()
}

#' retrofit_cost_model_power
#'
#' A retrofit fit cost model based a power law dependence of marginal upgrade cost on BER
#' on marginal cost function
#' \deqn{c_0 + \frac{k}{BER^\alpha}}
#' so that the cost of upgrade from \eqn{BER_{old}} to \eqn{BER_{new}} is
#' \deqn{c_o(BER_{old}-BER{_new}) + \frac{k}{1-\alpha}\left(BER_{old}^{1-\alpha}-BER_{new}^{1-\alpha}\right)}
#' The parameters \eqn{k} and \eqn{\alpha} are set in \emph{params}
#'
#' Default fit is based on cost matrix in ber_upgrade_cost_matrix. Assumes a square root dependence of cost on floor area (economy of scale)
#'
#' @param ber_old old ber kWh/m2
#' @param ber_new new ber kWh/m2
#' @param house_type seai house type
#' @param region region
#' @param floor_area total floor area in m2
#' @param params current parameter values
#'
#' @returns cost in euros
#' @export
#'
#' @examples
#'
#' params <- scenario_params(sD,2018)
#' retrofit_cost_model_power(200,100,"semi_detached","Dublin",100,params)
#'
#' params <- scenario_params(sD,2024)
#' retrofit_cost_model_power(200,100,"semi_detached","Dublin",100,params)
#'
retrofit_cost_model_power <- function(ber_old,ber_new,house_type,region="Dublin",floor_area=100,params){

  c_0 <- params$ber_upgrade_marginal_cost_c0
  alpha <- params$ber_upgrade_marginal_cost_alpha
  k <- params$ber_upgrade_marginal_cost_k
  cost <- ifelse(ber_old > ber_new,c_0*(ber_old-ber_new) + k/(1-alpha)*(ber_old^(1-alpha)-ber_new^(1-alpha)),0)
  res_2024 <- 10*cost*sqrt(floor_area)*ifelse(house_type=="apartment",0.75,1)*ifelse(region=="Dublin",1.25,1)
  #adjust by 2024 labour cost
  res_2024*((1-params$ber_upgrade_labour_cost_share) + params$ber_upgrade_labour_cost_share*params$labour_cost/60) %>% return()
  #include premium for detached vs semi-d vs terraced
}


retrofit_cost_model_power2 <- function(hli_old,hli_new,house_type,region="Dublin",floor_area=100,params){

  c_0 <- params$ber_upgrade_marginal_cost_c0
  alpha <- params$ber_upgrade_marginal_cost_alpha
  k <- params$ber_upgrade_marginal_cost_k
  cost <- ifelse(hli_old > hli_new,c_0*(hli_old-hli_new) + k/(1-alpha)*(hli_old^(1-alpha)-hli_new^(1-alpha)),0)
  res_2024 <- 10*cost*sqrt(floor_area)*ifelse(house_type=="apartment",0.75,1)*ifelse(region=="Dublin",1.25,1)
  #adjust by 2024 labour cost
  res_2024*((1-params$ber_upgrade_labour_cost_share) + params$ber_upgrade_labour_cost_share*params$labour_cost/60) %>% return()
  #include premium for detached vs semi-d vs terraced
}


#' retrofit_cost_model_logistic
#'
#' Building fabric + ventilation retrofit upgrade cost bute excluding tech such as solar PV or Heat Pumps
#'
#' The default and possibly the most plausible cost model, based on HLI to exclude effects of heating technology efficiency.
#'
#' Economies of scale currently use a \eqn{area^\frac{2}{3}} scaling law.
#'
#'
#' @param hli_old hli before upgrade
#' @param hli_new hli after upgrade
#' @param house_type house type
#' @param storeys number of storeys
#' @param region region
#' @param floor_area floor area (m2)
#' @param params current parameters
#'
#' @returns euros
#' @export
#'
#' @examples
#' params <- scenario_params(sD,2018)
#' retrofit_cost_model_logistic(5,4,"semi_detached",2,"Dublin",100,params)
#'
#' params <- scenario_params(sD,2024)
#' retrofit_cost_model_logistic(4.5,2.0,"detached",2,"Munster",100,params)
#' retrofit_cost_model_logistic(2.41,1.91,"detached",1,"Dublin",175,params) #UCD example
#'
retrofit_cost_model_logistic <- function(hli_old,hli_new,house_type,storeys,region="Dublin",floor_area=100,params){

  #paramters from scratch.R
  storeys <- ifelse(house_type=="apartment",1,storeys) #assume apartments are 1 storey
  #cost model params for 2+ storeys are the same
  model_params <- upgrade_logistic_cost_model %>% dplyr::mutate(storeys=pmin(storeys,2)) %>%  dplyr::filter(dwelling_type==house_type, no_storeys==storeys)
  k <-  model_params$k
  c_min <- model_params$c_min
  c_max <- model_params$c_max*1.1
  h_0 <- model_params$h_0
  #marginal_cost <- function(h) c_min + (c_max-c_min)/(1+exp(k*(h-h_0)))
  #cost <- ifelse(ber_old > ber_new,integrate(marginal_cost,lower=ber_new,upper=ber_old)$value,0)
  cost <- ifelse(hli_old > hli_new, c_max*(hli_old-hli_new) + (c_max-c_min)/k*(log1p(exp(k*(hli_new-h_0))) - log1p(exp(k*(hli_old-h_0)))),0)
  #res_2024 <- 10*cost*sqrt(floor_area)*ifelse(region=="Dublin",1.25,1)
  res_2024 <- 4.641589*cost*floor_area^(2/3)*ifelse(region=="Dublin",1.25,1) #economy of scale & Dublin premium (Kren et al)
  #adjust by 2024 labour cost
  res_2024*((1-params$ber_upgrade_labour_cost_share) + params$ber_upgrade_labour_cost_share*params$labour_cost/60) %>% return()

  #include premium for detached vs semi-d vs terraced
}


#' retrofit_cost_model
#'
#' energy retrofit upgrade cost model collection
#'
#' @param hli_old old hli
#' @param hli_new new hli
#' @param house_type house type
#' @param storeys 1, 2+
#' @param region region
#' @param floor_area floor area (m2)
#' @param cost_model one of "esri","logistic","power"
#' @param params current parameters
#'
#' @returns euros
#' @export
#'
#' @examples
#'
#' params <- scenario_params(sD,2026)
#' retrofit_cost_model(2.8,1.8,"detached",2,"Dublin",120,"logistic",params)
#'
retrofit_cost_model <- function(hli_old,hli_new,house_type,storeys,region="Dublin",floor_area=100,cost_model="logistic",params){

  stopifnot(cost_model %in% c("logistic","power"))
  storeys <- pmin(storeys,2)
  res <- if(cost_model=="power") retrofit_cost_model_power(ber_old,ber_new,house_type,region,floor_area,params)
  else
    if(cost_model=="logistic") retrofit_cost_model_logistic(hli_old,hli_new,house_type,storeys,region,floor_area,params)
  else retrofit_cost_model_esri(ber_old,ber_new,house_type,region,floor_area,params)
 res %>% return()
}

#' gen_upgrade_cost_matrix
#'
#' generates an implied BER upgrade for given cost model params and house_type, region and floor area
#'
#' @param house_type house or apartment
#' @param storeys 1,2+
#' @param region region
#' @param floor_area total floor area in m2
#' @param params parameters
#' @param cost_model current choice "marginal" or "esri" or "logistic"
#' @param include_grant TRUE/FALSE
#'
#' @returns matrix
#' @export
#'
#' @examples
#'
#' params <- scenario_params(sD,2024)
#' #gen_upgrade_cost_matrix("semi_detached","Dublin",100,params,cost_model="power",FALSE)
#' #gen_upgrade_cost_matrix("semi_detached","Dublin",100,params,cost_model="esri",FALSE)
#' gen_upgrade_cost_matrix("semi_detached",2,"Dublin",100,params,cost_model="logistic",FALSE)
#'
gen_upgrade_cost_matrix <- function(house_type,storeys,region="Dublin",floor_area=100,params,cost_model="logistic",include_grant=FALSE){
  #gen_upgrade_cost_matrix("semi_detached","Dublin",100,params,"marginal")
  stopifnot(house_type %in% c("detached","semi_detached","apartment","terraced"))
  stopifnot(cost_model %in% c("power","esri","logistic"))
  storeys <- pmin(storeys,2)
  df <- tidyr::expand_grid(hli_old=seq(5,0.5,by=-0.5), hli_new=seq(5,0.5,by=-0.5))
  df <- df %>% dplyr::rowwise() %>% dplyr::mutate(cost = retrofit_cost_model(hli_old,hli_new,house_type,storeys,region,floor_area,cost_model,params))
  #df <- df %>% dplyr::mutate(old_ber_score = get_ber_score(ber_old),new_ber_score=get_ber_score(ber_new))
  #df <- df %>% dplyr::group_by(old_ber_score,new_ber_score) %>% dplyr::summarise(cost=mean(cost))
  df <- df %>% tidyr::pivot_wider(id_cols="hli_old",names_from="hli_new",values_from=cost)
  colnames <- df$hli_old
  df <- df %>% dplyr::ungroup() %>%  dplyr::select(-hli_old)
  colnames(df) <- colnames
  df <- tibble::tibble(hli_old = colnames) %>% dplyr::bind_cols(df)
  return(df)
}

#' heating_upgrade_tensor
#'
#' This is a core function of hpmicrosimr. heating_upgrade_tensor calculates an element of the heating system upgrade cost tensor
#' i.e. the costs and cost savings (hli_old,tech_old) -> (hli_new,tech_new)
#'
#' fabric upgrade costs are discounted with infinite lifetime. Present bias (params$beta.) is included for new investments. An additional sludge/
#'
#' the new tech is installed at current time (params$yeartime). The old tech installation time is also specified.
#'
#' All applicable grants used by default, the effects of grants can optionally be excluded by setting include_grants=FALSE
#'
#' Optionally, the current (old) heating system can be retained
#'
#' Used with optimise_upgrade to determine the optimal upgrade path tech_old -> tech_new and hli_old -> hli_new
#'
#' @param hli_old old ber double
#' @param hli_new new ber double
#' @param tech_old old tech
#' @param installation_time time of installation of current system (for estimate of old operating cost only)
#' @param tech_new new tech
#' @param house_type house type in seai codes
#' @param storeys number of storeys 1, 2+
#' @param construction_year year,integer
#' @param region region
#' @param floor_area floor area of property in m2
#' @param cost_model ber upgrade cost model (power, logistic or esri) default logistic
#' @param params params
#' @param upgrade_heat TRUE if current heating system is upgraded
#' @param is_fuel_allowance TRUE/FALSE
#' @param include_grants TRUE/FALSE
#' @param include_rebound defulat FALSE
#'
#' @returns list
#' @export
#'
#' @examples
#'
#' params <- scenario_params(sD,2026)
#'
#' heating_upgrade_tensor(4,2,"gas",2010,"gas","semi_detached",2,1990,"Dublin",100,cost_model="logistic",params,TRUE,FALSE,TRUE)
#'
#' heating_upgrade_tensor(4,3,"oil",2000,"heat_pump","terraced",2,1980,"Munster",90,"logistic",params,TRUE,FALSE,TRUE,include_rebound=TRUE)
#'
#' heating_upgrade_tensor(3,2,"oil",2000,"heat_pump","semi_detached",2, 1980,"Munster",100,"logistic",params,TRUE,FALSE,TRUE,include_rebound=FALSE)
#'
#' heating_upgrade_tensor(2,1,"gas",2000,"gas","detached",2, 1980,"Dublin",100,"logistic",params,TRUE,FALSE,TRUE,include_rebound=FALSE)
#' heating_upgrade_tensor(3,2,"gas",2000,"gas","detached",2, 1980,"Dublin",100,"logistic",params,TRUE,FALSE,TRUE,include_rebound=FALSE)
#' heating_upgrade_tensor(3,1,"gas",2000,"gas","detached",2, 1980,"Dublin",100,"logistic",params,TRUE,FALSE,TRUE,include_rebound=FALSE)

#' ber_from_hli(1.27,"gas",params$yeartime,params)

heating_upgrade_tensor <- function(hli_old,hli_new,tech_old,installation_time,tech_new,house_type,storeys,construction_year,region,floor_area,cost_model="logistic",params,upgrade_heat=FALSE, is_fuel_allowance=FALSE,include_grants=TRUE, include_rebound=FALSE){
  #heating_upgrade_tensor(175,120,tech_old="gas",tech_new = "heat_pump","detached",2003,"Dublin",100,params,include_grants=T)
  stopifnot(tech_old %in% c("electricity","heat_pump","solid_fuel","gas","oil"))
  stopifnot(tech_new %in% c("electricity","heat_pump","solid_fuel","gas","oil"))
  stopifnot(house_type %in% c("apartment","terraced","semi_detached","detached"))
  #
  installation_type <- ifelse(tech_old == tech_new,"swap","new")
  #grant eligibility is based on ber_new or ber_uplift
  ber_old <- ber_from_hli(hli_old,tech_old,installation_time,params)
  ber_new <- ber_from_hli(hli_new,tech_new,params$yeartime,params) #includes the impact of heat_pumps or improved efficiency
  #old_annualised_cost <- heating_system_operating_cost(tech_old,ber_old*floor_area,params)
  grant_type <- ifelse(include_grants, grant_eligibility(ber_old,ber_new,construction_year,is_fuel_allowance,params), "None")
  present_bias <- params$beta.
  #The old annualised cost should not include present bias
  params$beta. < 1
  old_annualised_cost <- annualised_heating_system_cost(hli_old,tech_old,installation_time,installation_type="swap",floor_area,house_type,construction_year,grant_type="None",params,include_rebound)
  #restore present bias
  params$beta. <- present_bias
  #ber upgrade cost including ber grants but excluding heat pump grants
  fabric_grants <- fabric_grant(ber_old,ber_new,tech_old,tech_new,installation_time,construction_year,region,house_type,storeys,floor_area,is_fuel_allowance,cost_model,params)
  #print(upgrade_grants$scheme)
  hli_upgrade_capex <- fabric_grants$cost_estimate
  #hli_upgrade_capex_with_grant <-  hli_upgrade_capex-ifelse(include_grants,upgrade_grants$grant_value,0) #
  hli_upgrade_capex_with_grant <- hli_upgrade_capex - fabric_grants$grant_value
  #include present bias & grant effect
  hli_upgrade_annualised_capex <- ifelse(hli_upgrade_capex_with_grant <= params$present_bias_threshold, hli_upgrade_capex_with_grant*params$r.,(params$present_bias_threshold + (hli_upgrade_capex_with_grant-params$present_bias_threshold)/params$beta.)*params$r.)
  #ber_upgrade_annualised_capex <- ber_upgrade_capex*params$r.
  #grant_type <- ifelse(include_grants,upgrade_grants$scheme,"None")
  #print(upgrade_grants$grant_value)
  if(upgrade_heat){
   heating_req <- space_heating_requirement(hli_new,floor_area,rebound=0,params) #installer assumption with no rebound!
   kW_new <- peak_heating_demand(hli_new,floor_area)
   #heating system if house already has a heat pump, not eligible for a grant
   # no heat pump grant if hli_new > params$
   heat_grant_type <- if(tech_old=="heat_pump"| hli_new > params$hli_heat_pump_threshold | tech_new != "heat_pump" | !include_grants) {"None"} else {grant_type}
   heating_sys_cost <- heating_system_capital_cost(tech_new,kW_new,installation_type,house_type,construction_year,heat_grant_type,params,include_vat = TRUE)$cost
   #for now assume there is only one discount rate
   #infinite lifetime
   #should the heating system be retained or upgraded at this time?
   new_heat_annualised_cost <- annualised_heating_system_cost(hli_new,tech_new,params$yeartime,installation_type,floor_area,house_type,construction_year,heat_grant_type,params,include_rebound)
   new_annualised_cost <- new_heat_annualised_cost + hli_upgrade_annualised_capex
  #if the new tech is a heatpump consider the BetterEnergyHomes grant
  #if(tech_new=="heat_pump" & upgrade_grants$scheme=="None") upgrade_grants$scheme <- "BetterEnergyHomes"
  hp_grant <- heat_pump_grant(installation_type,house_type,construction_year,heat_grant_type,params)
  if(hp_grant=="cost") hp_grant <-  heating_sys_cost
  #print(hp_grant)
  #c("old"=old_annualised_cost, "new"=new_annualised_cost, "loss_or_gain"=new_annualised_cost-old_annualised_cost)
  res <- list("new_cost"=as.numeric(new_annualised_cost),"old_cost"=as.numeric(old_annualised_cost),"savings"= (new_annualised_cost/old_annualised_cost-1), "upgrade_cost"=as.numeric(hli_upgrade_capex),"heating_sys_cost"=heating_sys_cost,"grant_type"=grant_type,"upgrade_grant"=as.numeric(fabric_grants$grant_value),"heat_pump_grant"=as.numeric(hp_grant))
  res %>% tibble::as_tibble() %>% return()
  }
  else
  {
    #print("no heat upgrade - retain existing heating installation")
    heating_req <- space_heating_requirement(hli_old,floor_area,rebound=0,params) #no misery assumption
    kW_old <- peak_heating_demand(hli_old,floor_area)
    #heating system if house already has a heat pump, not eligible for a grant
    params_old <- scenario_params(sD,installation_time)
    old_grant_type <- ifelse(include_grants & tech_old=="heat_pump" & hli_old < params$hli_heat_pump_threshold,  grant_eligibility(ber_old,ber_new,construction_year,is_fuel_allowance,params_old),"None")
    heating_sys_cost <- heating_system_capital_cost(tech_old,kW_old,"new",house_type,construction_year,old_grant_type,params_old,include_vat = TRUE)$cost
    #evluate heating system cost with old capital cost and current fuel system
    new_heat_annualised_cost <- annualised_heating_system_cost(hli_new,tech_old,installation_time,"new",floor_area,house_type,construction_year,old_grant_type,params,include_rebound)
    new_annualised_cost <- new_heat_annualised_cost + hli_upgrade_annualised_capex_with_grant
    #if the new tech is a heatpump consider the BetterEnergyHomes grant
    #if(tech_new=="heat_pump" & upgrade_grants$scheme=="None") upgrade_grants$scheme <- "BetterEnergyHomes"
    hp_grant <- ifelse(tech_new=="heat_pump",heat_pump_grant(installation_type,house_type,construction_year,old_grant_type,params_old),0)
    if(hp_grant=="cost") hp_grant <-  heating_sys_cost
    #print(hp_grant)
    #c("old"=old_annualised_cost, "new"=new_annualised_cost, "loss_or_gain"=new_annualised_cost-old_annualised_cost)
    res <- list("new_cost"=as.numeric(new_annualised_cost),"old_cost"=as.numeric(old_annualised_cost),"savings"= (new_annualised_cost/old_annualised_cost-1), "upgrade_cost"=as.numeric(fabric_grants$cost_estimate),"heating_sys_cost"=heating_sys_cost,"grant_type"=grant_type,"upgrade_grant"=as.numeric(fabric_grants$grant_value),"heat_pump_grant"=as.numeric(hp_grant))
    res %>% tibble::as_tibble() %>% return()
  }
}
#

#' optimise_upgrade
#'
#' optimise_upgrade finds the financially optimum household energy efficiency upgrade (fabric and replacement heating technology)
#'
#' The potential new heating technologies (tech_new) considered are current tech and a heat pump. Choices such as gas to oil are not considered,
#'
#' If fuel_allowance is TRUE and the household qualifies for WarmerHomes. It is assumed that the property is upgraded to B2 (120 kWh/m2/y) standard.
#'
#' All grant_types apart from WarmerHomes requires BER optimisation.
#'
#' Returns a 2 row dataframe with tech_new the existing tech or a heat pump. If the existing tech is a heat pump, reversion to gas is also evaluated.
#'
#' @param hli_old heat loss indicator before upgrade W/Km2
#' @param tech_old current heating technology
#' @param installation_time year of installation of old (current) tech
#' @param house_type seai house type (detached, semi_detached, terraced, apartment)
#' @param storeys number of storeys 1, 2, 3
#' @param construction_year year of construction integer
#' @param region region (Dublin, Munster, Rest of Leinster, Ulster/Connacht)
#' @param floor_area treated floor area (m2)
#' @param cost_model efficiency upgrade cost model - only "logistic" at the moment
#' @param params current parameter values from scenario_params(sD, yeartime)
#' @param upgrade_heat if TRUE then upgrade the current heating system
#' @param is_fuel_allowance TRUE/FALSE
#' @param include_grants defaults to TRUE
#' @param include_rebound defaults to FALSE
#'
#' @returns a two row dataframe. The new EAC "new_cost"
#' @export
#'
#' @examples
#'
#' params <- scenario_params(sD,2026)
#'
#' optimise_upgrade(4,"gas",2010,"detached",2,2003,"Dublin",100,"logistic",params,TRUE,FALSE,TRUE)
#'
#' optimise_upgrade(3,"gas",2010,"detached",2,2010,"Dublin",100,"logistic",params,TRUE,FALSE,TRUE)
#'
#' optimise_upgrade(2.4,"gas",2010,"detached",2,2010,"Dublin",175,"logistic",params,TRUE,FALSE,TRUE)
#'
#' optimise_upgrade(2.9,"gas", 2015, "detached",2, 2003, "Munster", 100,"logistic",params,upgrade_heat=TRUE,is_fuel_allowance=FALSE,include_rebound=FALSE)
#'
#' optimise_upgrade(2.9,"gas", 2015, "detached",2, 2003, "Munster", 200,"logistic",params,upgrade_heat=TRUE,is_fuel_allowance=FALSE,include_rebound=FALSE)
#'
#' optimise_upgrade(3,"gas",2000,"detached",2,1990,"Dublin",120,"logistic",params,upgrade_heat=TRUE,is_fuel_allowance=FALSE,include_grants=TRUE)
#'
#' #WarmerHomes
#'
#' optimise_upgrade(4,"gas", 2015, "detached",2, 2003, "Munster", 100,"logistic",params,upgrade_heat=TRUE,is_fuel_allowance=TRUE,include_rebound=FALSE)
#'
#'
optimise_upgrade <- function(hli_old,tech_old,installation_time,house_type,storeys,construction_year,region,floor_area,cost_model="logistic",params,upgrade_heat=TRUE,is_fuel_allowance=FALSE,include_grants=TRUE,include_rebound=FALSE){

  stopifnot(construction_year %in% 1700:2025)
  storeys <- pmin(storeys,2)
  stopifnot(storeys %in% 1:2)
  #
  ber_old <- ber_from_hli(hli_old,tech_old,installation_time,params)
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
    newtechs <- ifelse(tech_old=="heat_pump",list(c("heat_pump","gas")), list(c(tech_old,"heat_pump")))[[1]]
    for(tech_new in newtechs){
     ber_new <- params$warmer_homes_target_ber
     hli_new <- heat_loss_indicator(ber_new,tech_new,params$yeartime,params)
     df1 <- tibble::tibble(tech_old=tech_old,tech_new = tech_new,hli_old=hli_old,hli_new=hli_new,ber_old=ber_old,ber_new =120) %>% dplyr::bind_cols(heating_upgrade_tensor(hli_old,hli_new,tech_old,installation_time,tech_new,house_type,storeys,construction_year,region,floor_area,cost_model,params,upgrade_heat,is_fuel_allowance,include_grants=TRUE))
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
  newtechs <- ifelse(tech_old=="heat_pump",list(c("heat_pump","gas")), list(c(tech_old,"heat_pump")))[[1]]
  for(tech_new in newtechs){
  #print(tech_new)
  fun <- function(hli_new){

    heating_upgrade_tensor(hli_old,hli_new,tech_old,installation_time,tech_new, house_type,storeys,construction_year,region,floor_area,cost_model,params,upgrade_heat,is_fuel_allowance=FALSE,include_grants,include_rebound)$new_cost

  }

   result <- optim(par=hli_old,fn=fun,lower=0.2,upper=hli_old,method="Brent")
   hli_new <- result$par
   ber_new <- ber_from_hli(hli_new,tech_new,params$yeartime,params)
   df <- df %>% dplyr::bind_rows(tibble::tibble(tech_old=tech_old,tech_new=tech_new,hli_old=hli_old,hli_new=hli_new,ber_old=ber_old,ber_new=ber_new))
  }

  df <- df %>% dplyr::rowwise() %>% dplyr::mutate(result = list(heating_upgrade_tensor(hli_old,hli_new,tech_old,installation_time,
                                                                                       tech_new,house_type,storeys,construction_year,
                                                                                       region,floor_area,cost_model,params,upgrade_heat,is_fuel_allowance=FALSE,include_grants,include_rebound))) %>% tidyr::unnest_wider(result)
 #df <- df %>% dplyr::mutate(bill_savings = 100*(annualised_cost-annualised_cost_old)/annualised_cost_old)
  df <- df %>% dplyr::filter(tech_new %in% newtechs) #%>% dplyr::slice_min(new_cost)
  }
  return(df %>% dplyr::distinct())
  }
  else{ #no heating system upgrade
    #First check whether qualifies for WarmerHomes
      if(grant_type=="WarmerHomes"){
        #WarmerHomes is assume to retain current heating system
        #tech_new <- ifelse(params$yeartime >= 2025,"heat_pump",tech_old)
        #df <- tibble::tibble(tech_old=tech_old,tech_new=c(tech_old,"heat_pump"), ber_old=ber_old,ber_new=120)
        df <- tibble::tibble()
        newtechs <- ifelse(tech_old=="heat_pump",list(c("heat_pump","gas")), list(c(tech_old,"heat_pump")))[[1]]
        for(tech_new in newtechs){
          #
          hli_new <- heat_loss_indicator(params$warmer_homes_target_ber,tech_new,params$yeartime,params)
          df1 <- tibble::tibble(tech_old=tech_old,tech_new = tech_old,hli_old=hli_old,hli_new=hli_new,ber_old=ber_old,ber_new =params$warmer_homes_target_ber) %>% dplyr::bind_cols(heating_upgrade_tensor(hli_old,hli_new,tech_old,installation_time,tech_new,house_type,storeys,construction_year,region,floor_area,params,upgrade_heat,is_fuel_allowance=TRUE,include_grants))
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

          fun <- function(hli_new){

            heating_upgrade_tensor(hli_old,hli_new,tech_old,installation_time,tech_old, house_type,storeys,construction_year,region,floor_area,params,upgrade_heat=TRUE,is_fuel_allowance=FALSE,include_grants,include_rebound)$new_cost

          }

          result <- optim(par=hli_old,fn=fun,lower=0.1,upper=hli_old,method="Brent")
          hli_new <- result$par
          ber_new <- ber_from_hli(hli_new,tech_new,params$yeartime,params)
          df <- df %>% dplyr::bind_rows(tibble::tibble(tech_old=tech_old,tech_new=tech_old, hli_old=hli_old,hli_new=hli_new,ber_old=ber_old,ber_new=ber_optimal))
        }

        df <- df %>% dplyr::rowwise() %>% dplyr::mutate(result = list(heating_upgrade_tensor(hli_old,hli_new,tech_old,installation_time,
                                                                                             tech_old,house_type,storeys,construction_year,
                                                                                             region,floor_area,params,upgrade_heat,FALSE,include_grants,include_rebound))) %>% tidyr::unnest_wider(result)
        #df <- df %>% dplyr::mutate(bill_savings = 100*(annualised_cost-annualised_cost_old)/annualised_cost_old)
        #df <- df %>% dplyr::filter(tech_new %in% c(tech_old,"heat_pump")) #%>% dplyr::slice_min(new_cost)
      return(df %>% dplyr::distinct())

  }
}

#' optimise_heat
#'
#' Find the financially optimum household heating system replacement with no BER upgrade. The choice is between replacing current system or installing a heat pump.
#'
#' The function is used for example in the case of a heating system failure, where a household does not consider heat loss efficiency upgrades.
#'
#' If current system is a heat pump, choose between a heat pump swap or reversion to gas.
#'
#' @param hli the current W/Km2 not upgraded
#' @param tech_old new ber kWh/m2/year
#' @param installation_time year of installation of old tech
#' @param house_type seai house type
#' @param storeys 1 or 2+
#' @param construction_year integer
#' @param region region
#' @param floor_area total floor area (m2)
#' @param params current parameter values
#' @param include_rebound default FALSE
#'
#' @returns data frame (2 or 1 rows)
#' @export
#'
#' @examples
#' params <- scenario_params(sD,2026)
#' optimise_heat(2,"oil",2000,"terraced",2,1980,"Munster",90,params)
#'
#' params <- scenario_params(sD,2026)
#' optimise_heat(4,"oil",2000,"terraced",2,1980,"Munster",90,params)
#' optimise_heat(1.9,"oil",2000,"terraced",2,1980,"Munster",90,params)
#'
optimise_heat <- function(hli,tech_old,installation_time,house_type,storeys,construction_year,region,floor_area,params,include_rebound=FALSE){
  #optimise_upgrade(ber_old=175,tech_old = "oil",house_type="detached",2003,region="Munster",floor_area=100,params)
  newtechs <- ifelse(tech_old=="heat_pump",list(c("heat_pump","gas")), list(c(tech_old,"heat_pump")))[[1]]

  df0 <- tibble::tibble("tech_old"=tech_old,"tech_new"=newtechs)
  upgrade_fun <- function(tech_old, tech_new){
        heating_upgrade_tensor(hli,hli,tech_old,installation_time,tech_new,house_type,storeys,construction_year,region,floor_area,"logistic",params,TRUE,FALSE,TRUE,include_rebound)
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
#' The "savings" are relative to an replacing the current system with a new system with the same technology e.g. gas -> gas
#'
#' If current system is a heat pump, choose between a heat pump swap or reversion to gas.
#'
#' This function is used in update_agents to determine the system choice when current heating system fails
#'
#' @param hli current HLI
#' @param tech_old current tech
#' @param installation_time year of installation of old tech
#' @param house_type seai house type
#' @param storeys 1 or 2+
#' @param construction_year integer
#' @param region region
#' @param floor_area total floor area (m2)
#' @param params current parameter values
#'
#' @returns one row dataframe
#' @export
#'
#' @examples
#' params <- scenario_params(sD,2025)
#' heat_pump_savings(4,"gas",2010,"detached",2,1995,"Munster",120,params)
#' params$beta. <- 1
#' heat_pump_savings(3,"solid_fuel",2010,"detached",2,1995,"Munster",100,params)
#'
#' heat_pump_savings(4,"heat_pump",2010,"detached",2,1995,"Munster",120,params)

heat_pump_savings <- function(hli,tech_old,installation_time,house_type,storeys,construction_year,region,floor_area,params){
  #
  df <- optimise_heat(hli,tech_old,installation_time,house_type,storeys,construction_year,region,floor_area,params,include_rebound=FALSE)
  stick_cost <- df %>% dplyr::filter(tech_old==tech_new) %>% dplyr::pull(new_cost)
  switch_cost <- df %>% dplyr::filter(tech_old!=tech_new) %>% dplyr::pull(new_cost)
  #switch_cost <- ifelse(tech_old=="heat_pump",NA,df %>% dplyr::filter(tech_old!=tech_new) %>% dplyr::pull(new_cost))
  #savings <- ifelse(tech_old=="heat_pump",NA, switch_cost/stick_cost -1)
  grant <- df %>% dplyr::filter(tech_new=="heat_pump") %>% dplyr::pull(heat_pump_grant)
  grant_type <- df %>% dplyr::filter(tech_new=="heat_pump") %>% dplyr::pull(grant_type)
  tibble::tibble(eac_stick=stick_cost, eac_switch=switch_cost,savings=switch_cost/stick_cost -1, hp_grant_type=grant_type,heat_pump_grant = grant) %>% return()
}


#' heat_pump_upgrade_savings
#'
#' % savings achieved by adopting a heat pump as part of an home energy upgrade, expressed relative to retaining the current technology.
#'
#' If the relative gain is sufficient, a heat pump may be adopted.
#'
#' Returns the savings by switching and ber
#'
#'
#' @param hli_old starting hli
#' @param tech_old old tech
#' @param installation_time time of installation of old tech
#' @param house_type seai house type
#' @param storeys 1 or 2+
#' @param construction_year house construction year
#' @param region region
#' @param floor_area floor area in m2
#' @param cost_model logistic, power or esri
#' @param params scenario params at current yeartime
#' @param is_fuel_allowance default FALSE
#' @param include_grants TRUE or FALSE
#'
#' @returns a single row dataframe
#' @export
#'
#' @examples
#'
#' params <- scenario_params(sD,2026)
#' heat_pump_upgrade_savings(5,"gas",2000,"detached",2,1990,"Munster",120,"logistic",params,is_fuel_allowance=FALSE,include_grants=TRUE)
#'
heat_pump_upgrade_savings <-  function(hli_old,tech_old,installation_time,house_type,storeys,construction_year,region,floor_area,cost_model="logistic",params,is_fuel_allowance,include_grants){
  #
  df <- optimise_upgrade(hli_old,tech_old,installation_time,house_type,storeys,construction_year,region,floor_area,cost_model,params,upgrade_heat=TRUE,is_fuel_allowance,include_grants,include_rebound=FALSE)
  #
  df_stick <- df %>% dplyr::filter(tech_new == tech_old) %>% dplyr::select(hli_new,new_cost,upgrade_cost,heating_sys_cost, grant_type,upgrade_grant, heat_pump_grant)
  df_switch <- df %>% dplyr::filter(tech_new != tech_old) %>% dplyr::select(hli_new,new_cost,upgrade_cost,heating_sys_cost, grant_type,upgrade_grant, heat_pump_grant)
  df_stick <- df_stick %>% dplyr::rename("hli"=hli_new,"eac"=new_cost)
  df_switch <- df_switch %>% dplyr::rename("hli"=hli_new,"eac"=new_cost)
  df_stick <- df_stick %>% dplyr::rename_with(~ paste0(.x,"_stick"))
  df_switch <- df_switch %>% dplyr::rename_with(~ paste0(.x,"_switch"))
  df1 <- df_stick %>% dplyr::bind_cols(df_switch)
  df1 <- df1 %>% dplyr::mutate(savings=eac_switch/eac_switch-1)
  #df1 <- df1 %>% dplyr::rename("tech"=tech_old_stick)
  #switch_ber <- ifelse(tech_old=="heat_pump",NA,df %>% dplyr::filter(tech_old!=tech_new) %>% dplyr::pull(ber_new))
  df1 %>% return()
}


#' grant_eligibility
#'
#' utility function to determine which home energy efficiency grant scheme a homeowner is or was eligible
#'
#' expressed in terms of ber_old and ber_new
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

#' fabric_grant
#'
#' find the grant amount available from SEAI for energy efficiency upgrades. This function *excludes* grants for heat pumps and associated works.
#'
#' The fabric grant is expressed in terms of ber_old and ber_new. HLI values are calculated
#'
#' the fabric grant depends on eligility for OSS and therefore on ber scores and uplift. This means that it depends also on tech_old and tech_new.
#'
#' @param ber_old old hli, double
#' @param ber_new new hli, double
#' @param tech_old old tech
#' @param heating_install_time decimal time
#' @param tech_new new tech
#' @param construction_year year of construction, integer
#' @param region region
#' @param house_type seai house type (related to q1)
#' @param storeys number of storeys 1, 2+
#' @param floor_area m2
#' @param is_fuel_allowance TRUE/FALSE
#' @param cost_model defaut logistic
#' @param params parameters
#' @param randomise whether to assign grant elements randomly or not. Default TRUE, for testing set to FALSE
#'
#' @returns list(scheme, grant)
#' @export
#'
#' @examples
#'
#' params <- scenario_params(sD,2025)
#' fabric_grant(300,200,"gas","gas",2010,2003,"Dublin","detached",2,100, FALSE,"logistic",params,randomise=TRUE)
#'
#' fabric_grant(300,150,"gas","gas",2010,2003,"Dublin","detached",2,100, FALSE,"logistic",params,randomise=TRUE)
#'
#' fabric_grant(300,100,"gas","gas",2010,2003,"Dublin","detached",2,100, FALSE,"logistic",params,randomise=TRUE)
#'
#' fabric_grant(150,40,"gas","gas",2010,2003,"Dublin","detached",2,100, is_fuel_allowance=TRUE,"logistic",params,randomise=TRUE)
#'
fabric_grant <- function(ber_old,ber_new,tech_old,tech_new,heating_install_time,construction_year,region,house_type,storeys,floor_area = 100,is_fuel_allowance = FALSE,cost_model="logistic",params,randomise=FALSE) {

  #Input validation with more informative messages
  stopifnot(house_type %in% c("semi_detached", "detached", "apartment", "terraced","apartment"))
  set.seed(as.integer(Sys.time()))
  hli_old <- heat_loss_indicator(ber_old,tech_old,heating_install_time,params)
  hli_new <- heat_loss_indicator(ber_new,tech_new,params$yeartime,params)
  cost_estimate <- retrofit_cost_model(hli_old,hli_new,house_type,storeys,region,floor_area,cost_model,params)
  scheme0 <- grant_eligibility(ber_old,ber_new,construction_year,is_fuel_allowance,params)
  #(print(scheme0)
  if(scheme0=="None") return(list(scheme=scheme0,grant_value=0,cost_estimate=cost_estimate, grant_share=0))
  #if(scheme0=="None") return(list(scheme=scheme0,grant_value=0)
  #print(cost_estimate)
  #in case of WarmerHomes cap grant at B2 (100)
  if(scheme0=="WarmerHomes") {
    hli_new <- heat_loss_indicator(params$warmer_homes_target_ber,tech_new,params$yeartime,params)
    cost_estimate <- retrofit_cost_model(hli_old,hli_new,house_type,storeys,region,floor_area,cost_model,params)
    return(list(scheme=scheme0,grant_value=cost_estimate,cost_estimate=cost_estimate, grant_share=1))
  }
  #if(scheme0=="WarmerHomes") return(list(scheme=scheme0,grant_value="cost")
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
  #return(list(scheme=scheme0,grant_value=max_grant)) #factor of 0.8 because not all measures will apply
  }
  if(!randomise){
   relevant_grants <- seai_grants_average %>% dplyr::filter(scheme==scheme0,building_type==house_type)
   relevant_grants <-  relevant_grants %>% dplyr::filter(!stringr::str_detect(measure,"heat_pump"))
   #relevant_grants_mean <- relevant_grants %>% dplyr::mutate(measure=replace(measure, str_detect(measure,"attic|rafter"),"roof"))
   #relevant_grants_mean <- relevant_grants %>% dplyr::mutate(measure=replace(measure, str_detect(measure,"wall"),"wall"))
   max_grant <- pmin(cost_estimate,pmax(0,sum(relevant_grants$grant)-2000))  #manual adjustment
   #print(max_grant/cost_estimate)
   return(list(scheme=scheme0,grant_value=max_grant,cost_estimate=cost_estimate, grant_share=max_grant/cost_estimate)) #factor of 0.8 because not all measures will apply
  }
  }

#' heat_pump_grant
#'
#' heat pump grant, depending on grant_type and installation time (params$yeartime)
#'
#' no grant applies of HLI is great than params$hli_heat_pump_threshold
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
#' heat_pump_grant("new","apartment",2003,"BetterEnergyHomes",scenario_params(sD,2024))
#' params <- scenario_params(sD,2025)
#' heat_pump_grant("new","semi_detached",1997,"OSS",params)
#' heat_pump_grant("new","semi_detached",1997,"BetterEnergyHomes",params)
#' heat_pump_grant("new","semi_detached",1997,"none",params)
#'
#' heat_pump_grant("new","detached",2003,"BetterEnergyHomes",scenario_params(sD,2024))

heat_pump_grant <- function(installation_type,house_type,construction_year,grant_type,params) {
  # Return grant amount based on date and type
  stopifnot(house_type %in% c("detached","semi_detached","terraced","apartment"))
  stopifnot(grant_type %in% c("None","none","BetterEnergyHomes","OSS","WarmerHomes"))
  if(params$yeartime < params$oss_introduction & grant_type=="OSS") stop("OSS does not exist yet, try BetterEnergyHomes")
  #if(grant_type=="OSS" & params$yeartime < params$hp_grant_increase) stop("OSS did not exist at this time")
  #for now assume no heat pump grants avaiable for WarmerHomes
  if(installation_type=="swap" | params$yeartime < params$hp_grant_introduction | params$yeartime > params$hp_grant_removal | construction_year > 2020 | grant_type %in% c("None","none")) return(0)
  # No grant before Q2 2018
  #assumes that heat pumps are not installed before as part of WarmerHomes before 2025.
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

