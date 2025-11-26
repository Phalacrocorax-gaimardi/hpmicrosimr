#ber_ratings_table <- readr::read_csv("C:/Users/Joe/pkgs/hpmicrosimr/inst/extdata/ber_ratings_table.csv")
#hp_qanda <- readr::read_csv("C:/Users/Joe/pkgs/hpmicrosimr/inst/extdata/hp_qanda.csv")
#hp_survey_oo <- readr::read_csv("C:/Users/Joe/pkgs/hpmicrosimr/inst/extdata/hp_survey_oo.csv")
#hp_questions <- readr::read_csv("C:/Users/Joe/pkgs/hpmicrosimr/inst/extdata/hp_questions.csv")
#heating_bill_values <- readr::read_csv("C:/Users/Joe/pkgs/hpmicrosimr/inst/extdata/heating_bill_values.csv")
#electricity_prices <- readr::read_csv("C:/Users/Joe/pkgs/hpmicrosimr/inst/extdata/electricity_prices.csv")
#oil_prices <- readr::read_csv("C:/Users/Joe/pkgs/hpmicrosimr/inst/extdata/oil_prices.csv")
#wood_prices <- readr::read_csv("C:/Users/Joe/pkgs/hpmicrosimr/inst/extdata/wood_prices.csv")
#gas_prices <- readr::read_csv("C:/Users/Joe/pkgs/hpmicrosimr/inst/extdata/gas_prices.csv")
#gas_prices$fuel<- "gas"
#oil_prices$fuel<- "oil"
#wood_prices$fuel<- "solid fuel"
#electricity_prices$fuel<- "electricity"
#energy_prices <- gas_prices %>% bind_rows(oil_prices,wood_prices,electricity_prices)
#survey_income_ranges <- readr::read_csv("C:/Users/Joe/pkgs/hpmicrosimr/inst/extdata/income_values.csv")
#fuel_allowance_eligibility <- readr::read_csv("C:/Users/Joe/pkgs/hpmicrosimr/inst/extdata/fuel_allowance_eligibility.csv")
#sample_construction_years <- readr::read_csv("inst/extdata/sample_construction_years.csv")
#weibull_params <- readr::read_csv("inst/extdata/weibull_params.csv")
#tech_failure_params <- readr::read_csv("inst/extdata/tech_failure_parameters.csv")
#seai_grants_average <- readr::read_csv("inst/extdata/seai_grants_averaged.csv")

#' get_ber_score
#'
#' get the BER score corresponding to a ber rating given in terms of kWh/m2/year
#' assuming 21C in living areas and 18C in other ares during the heating season
#'
#' @param ber_rating kWh/m2/year double
#'
#' @return character
#' @export
#'
#' @examples get_ber_score(133)
# Assumes ber_ratings_table has columns: min_rating, max_rating, code
get_ber_score <- function(ber_rating) {
  # Filter only once using which()
  match_idx <- which(ber_ratings_table$min_rating < ber_rating &
                       ber_ratings_table$max_rating >= ber_rating)

  if (length(match_idx) == 0) return(NA_character_)  # or handle gracefully

  codes <- ber_ratings_table$code[match_idx]
  codes[which.max(nchar(codes))]
}


#' gen_ber_rating
#'
#' Stochastically generate a ber rating (kWh/m2/year) from a ber score. G ratings are exponentially distributed from 450kWh/m2/year with
#' a default decay rate 0.01.
#'
#' @param ber_score character
#' @param g_rate exponential rate, defauly 1/100
#'
#' @returns double
#' @export
#'
#' @examples gen_ber_rating("B3")
gen_ber_rating <- function(ber_score, g_rate = 1/100){
  #
  ifelse(is.na(ber_score),return(NA),{
  min_rating <- ber_ratings_table %>% dplyr::filter(code==ber_score) %>% dplyr::pull(min_rating)
  max_rating <- ber_ratings_table %>% dplyr::filter(code==ber_score) %>% dplyr::pull(max_rating)

  ifelse(ber_score != "G", runif(1, min_rating,max_rating), min_rating+rexp(1,rate = g_rate))}
  )

}

gen_ber_rating <- Vectorize(gen_ber_rating)

#' impute_floor_area
#'
#' impute a floor area stochastically based on survey characteristics
#'
#' @param house_type property type
#' @param q2_0 number of storeys
#' @param region region (qc2)
#' @param q5_0 year property built
#' @param q3_0 number of bedrooms
#'
#'
#' @return double, ground floor area
#' @export
#'
#' @examples impute_floor_area("detached",3,"Dublin",2,4)

impute_floor_area <- function(house_type,q2_0,region,q5_0,q3_0){
  #
  if(house_type == "apartment") q2_0 <- 1
  q5_0 <- ifelse(q5_0==7,sample(1:4,1),q5_0) #assume construction year "don't know" means sometime before 2010
  area_params <- area_model_parameters %>% dplyr::ungroup() %>% dplyr::filter(q1==house_type,q2==q2_0,qc2==region,q5==q5_0,q3==q3_0)
  if(dim(area_params)[1]==0) area_params <- area_model_parameters %>% dplyr::ungroup() %>% dplyr::filter(house_type=="detached",q2==q2_0,qc2==region,q5==q5_0,q3==q3_0)
  mu <- area_params  %>% dplyr::pull(mu)
  sigma <- area_params %>% dplyr::pull(sigma)
  #this is needed because of lack of recent single storey semi-detached houses built after 2021
  if(identical(mu,numeric(0))){
    mu <- area_model_parameters %>% dplyr::ungroup() %>% dplyr::filter(q1==house_type,q2==q2_0,qc2==region,q5==q5_0-1,q3==q3_0) %>% dplyr::pull(mu)
    sigma <- area_model_parameters %>% dplyr::filter(q1==house_type,q2==q2_0,qc2==region,q5==q5_0-1,q3==q3_0) %>% dplyr::pull(sigma)
  }
  x <- exp(rnorm(1,mean=mu,sd=sigma))
  return(x)

}

impute_floor_area <- Vectorize(impute_floor_area)

#' impute_ber
#'
#' generate a missing Ber rating (double) based on household characteristics
#'
#' @param house_type property type
#' @param q2_0 number of storeys
#' @param region region
#' @param q5_0 year property built
#' @param q3_0 number of bedrooms
#'
#'
#' @return double, ber rating
#' @export
#'
#' @examples impute_ber("semi-detached",2,"Dublin",4,4)

impute_ber <- function(house_type,q2_0,region,q5_0,q3_0){
  #assume all "other" (usually bungalows) and flats are one storey : ignores maisonettes
  if(house_type == "apartment") q2_0 <- 1
  q5_0 <- ifelse(q5_0==7,sample(1:4,1),q5_0) #if "don't know" means sometime before 2010
  ber_params <- ber_model_parameters %>% dplyr::ungroup() %>% dplyr::filter(q1==house_type,q2==q2_0,qc2==region,q5==q5_0,q3==q3_0)
  mu <- ber_params %>% dplyr::pull(mu)
  sigma <- ber_params %>% dplyr::pull(sigma)
  #exp(rnorm(1,mean=mu,sd=sigma)) %>% as.numeric() %>% return()
  x <- rlnorm(1,mu,sigma)
  return(x)
}

impute_ber <- Vectorize(impute_ber)


#' recode_ber
#'
#' assign an A*,B*,C*,D*,E* BER code to a 2-digit BER code A1,A2, etc based on known distribution of BER codes. "Don't knows" are assigned to NA.
#'
#' @param ber_score single digit ber score, character
#'
#' @returns 2-digit ber score
#' @export
#'
#' @examples
recode_ber <- function(ber_score){

  dplyr::case_when(ber_score=="A"~paste("A",sample(1:3,1,prob = ber_distrib[1:3,]$share),sep=""),
                   ber_score=="B"~paste("B",sample(1:3,1,prob = ber_distrib[4:6,]$share),sep=""),
            ber_score=="C"~paste("C",sample(1:3,1,prob = ber_distrib[7:9,]$share),sep=""),
            ber_score=="D"~paste("D",sample(1:2,1,prob = ber_distrib[10:11,]$share),sep=""),
            ber_score=="E"~paste("E",sample(1:2,1,prob = ber_distrib[12:13,]$share),sep=""),
            ber_score=="F"~"F",
            ber_score=="G"~"G",
            ber_score=="Exempt from BER rating"~NA,
            ber_score=="It does not have a BER rating"~NA,ber_score=="It does not have a BER rating"~NA,
            nchar(ber_score)==2~ber_score)

}



#' recode_fuels
#'
#' this function recodes heating fuel types present in survey to oil,gas,electricity,heat_pump,solid_fuel. It also returns primary,secondary
#' and tertiary fuel types present. If these are not present, the fuel type is "none". recode_fuels also imputes the primary heating fuel type if this missing.
#'
#' solar thermal is set to "none" if it is present. If solar thermal is indicated as the primary fuel in the survey, then the secondary fuel is promoted.
#'
#' @param hp_data_in input survey data, usually based on hp_survey_oo
#'
#' @returns recoded dataframe with heating tech columns
#' @export
#'
#' @examples
#'
#' recode_fuels(hp_survey_oo)
#'
recode_fuels <- function(hp_data_in){


  recode_fun <- function(q11,z_secondary){

    techs <- c("oil","gas","electricity","solid_fuel","heat_pump","heat_pump","none",NA)
    primary <- dplyr::case_when(q11==1~"oil",
                                q11==2~"gas",
                                q11==3~"electricity",
                                q11==4~"solid_fuel",
                              #  q11==5~"solar_thermal",
                                q11 %in% 6:7~"heat_pump",
                                q11 %in% 8:9~NA)

    secondary <- techs[which(z_secondary==1)]
    res <- c("primary_heating"=primary,"secondary_heating"=secondary)
    if(is.na(res[3])) res[3] <- "none"
    if(is.na(res[2])) res[2] <- "none"

    return(res)

  }

   #recode solar thermal to "other"
   hp_data_in <- hp_data_in %>% dplyr::mutate(q11 = ifelse(q11==5,8,q11))

   hp_data_in1 <- hp_data_in %>% dplyr::ungroup() %>% dplyr::filter(!(q11 %in% 8:9))
   hp_data_in2 <- hp_data_in %>% dplyr::ungroup() %>% dplyr::filter((q11 %in% 8:9))
   #
   hp_data_in1$q11 <- as.factor(hp_data_in1$q11)
   suppressWarnings({
   rf <- ranger::ranger(q11~qb+qc2+q1+qe+q3+q4+q5+qf+q7+q13,hp_data_in1 ,num.trees=500,mtry=3, min.node.size = 1,sample.fraction = 0.8,
                       respect.unordered.factors = TRUE,replace=TRUE,max.depth = 1e+3,num.threads = 10,classification  = TRUE)
   })


   preds <- ranger::predictions(rf)

   q11_impute <- stats::predict(rf, data = hp_data_in2 %>% dplyr::select(-q11))

  qh_imputed <- q11_impute$predictions
  hp_data_in1$q11 <- hp_data_in1$q11 %>% as.character() %>% as.integer()
  hp_data_in2$q11 <- qh_imputed  %>% as.character() %>% as.integer()
  hp_data_out <- hp_data_in1 %>% dplyr::bind_rows(hp_data_in2)
  #z <- c(q121,q122,q123,q124,q126,q127,q128,q129)
  hp_data_out <- hp_data_out %>% dplyr::rowwise() %>% dplyr::mutate(
    primary_heat=recode_fun(q11,c(q121,q122,q123,q124,q126,q127,q128,q129))[1],
    secondary_heat1=recode_fun(q11,c(q121,q122,q123,q124,q126,q127,q128,q129))[2],
    secondary_heat2=recode_fun(q11,c(q121,q122,q123,q124,q126,q127,q128,q129))[3])

  return(hp_data_out)
}


recode_construction_year <- function(q5,house_type_region){


}


#' recode_survey
#'
#' recodes the input survey data for use in initialise_agents. The recoded features are BER, ground floor area, and gross household income.
#'
#' @param hp_data_in input survey e.g. hp_survey_oo
#' @param params parameter values
#'
#' @returns dataframe
#' @export
#'
#' @examples
#'
#' recode_survey(hp_survey_oo,scenario_params(sD,2015)) %>% system.time()
#'
recode_survey <- function(hp_data_in,params){


  hp_data_out <- hp_data_in %>% type.convert(as.is=TRUE)
  codes <- names(hp_data_out)
  #
  for(q_code in c("q6","qc2","q1")){
    if(!(q_code %in% codes)) next
    hp_response <- hp_qanda %>% dplyr::filter(question_code==q_code) %>% dplyr::select(response_code,response) %>% dplyr::rename(!!q_code:=response_code)
    hp_response <- hp_response %>% type.convert(as.is=TRUE)
    hp_data_out <- hp_data_out %>% dplyr::inner_join(hp_response) %>% dplyr::select(-!!rlang::sym(q_code)) %>% dplyr::rename(!!q_code:=response)

  }
  tech_fuel_dictionary <- c("electricity"="electricity","heat_pump"= "electricity","solid_fuel"="solid_fuel","gas"="gas","oil"="oil")
  house_type_dictionary <- c("Flat or apartment"="apartment","Terraced house"="terraced","Semi-detached house"="semi_detached","Detached house"="detached","Other"="detached")

  #recode fuels
  if("q11" %in% codes) hp_data_out <- recode_fuels(hp_data_out)
  #recode house_type
  if("q1" %in% codes) hp_data_out <- hp_data_out %>% dplyr::mutate(q1 = dplyr::recode(q1, !!!house_type_dictionary))

  #ber
  hp_data_out <- hp_data_out %>% dplyr::mutate(q6=replace(q6,nchar(q6)>2,NA))
  hp_data_out <- hp_data_out %>% dplyr::mutate(q6_b=recode_ber(q6))
  #hp_new <- hp_new %>% mutate(q6=gen_ber_rating(q6))
  hp_data_out <- hp_data_out %>% dplyr::rowwise() %>% dplyr::mutate(ber=ifelse(!is.na(q6_b),gen_ber_rating(q6_b),impute_ber(q1,q2,qc2,q5,q3)))
  #total floor area replaces ground floor area
  hp_data_out <- hp_data_out %>% dplyr::rowwise() %>%  dplyr::mutate(floor_area=q2*impute_floor_area(q1,q2,qc2,q5,q3)) %>% dplyr::select(-q6_b)

  #recode income
  if("qh" %in% codes) hp_data_out <- impute_hh_income(hp_data_out)
  #recode construction year to year value
  if("q5" %in% codes) hp_data_out <- hp_data_out %>% dplyr::rowwise() %>% dplyr::mutate(construction_year=recode_construction_year(q5))
  #heating system installation year
  if("q7" %in% codes) {hp_data_out <- hp_data_out %>% dplyr::rowwise() %>% dplyr::mutate(heating_install_time = 2025-heating_system_age(q7,primary_heat,params))}
  #hp_data_out <- hp_data_out %>% dplyr::rowwise() %>%  dplyr::mutate(heating_requirment=ber*ground_floor_area*q2)
  #if heating installation is before construction year then set heating installation to construction year
  hp_data_out <- hp_data_out %>% dplyr::mutate(heating_install_time =ifelse(heating_install_time < construction_year,construction_year,heating_install_time))
  #check fuel allowance eligibility
  hp_data_out <- hp_data_out %>% dplyr::mutate(fuel_allowance = is_eligible_fuel_allowance(actualage,income,qi))

  hp_data_out %>% return()
}


#hp_data_in <- hp_survey_oo %>% dplyr::select(q1,qc2,q2,q3,q5,q6,q7,q11,q13)
#test <- recode_survey(hp_data_in)



#' impute_hh_income
#'
#' imputes missing household income (qh) using data on car ownership from the household survey.
#' Uses random forest
#'
#' @param hp_data_in usually hp_survey_oo
#'
#' @returns input dataframe with imputed qh values
#' @export
#'
#' @examples impute_hh_income(hp_survey_oo)
impute_hh_income <- function(hp_data_in){

  zet_survey <- readxl::read_xlsx(system.file("extdata","ZET_survey_2024_values.xlsx",package="hpmicrosimr"),sheet=1)
  #include car ownership as an indicator of income
  rf <- ranger::ranger(qh~qb+qc2+q1+qe+q3+q4+qf+q13+qg+q58+q59_2,zet_survey %>% dplyr::filter(qh != 12),num.trees=500,mtry=3, min.node.size = 1,sample.fraction = 0.8, splitrule = "variance",
               respect.unordered.factors = TRUE,replace=TRUE,max.depth = 1e+3,num.threads = 10)
  #rf$r.squared


  preds <- ranger::predictions(rf)

  qh_impute <- stats::predict(rf, data = zet_survey %>% dplyr::select(-qh))

  zet_survey$qh_imputed <- qh_impute$predictions
  hp_data_out <- hp_data_in %>% dplyr::inner_join(zet_survey %>% dplyr::select(serial,qh_imputed))
  hp_data_out <- hp_data_out %>% dplyr::mutate(qh=ifelse(qh == 12, round(qh_imputed), qh))
  hp_data_out <- hp_data_out %>% dplyr::inner_join(survey_income_ranges)
  hp_data_out <- hp_data_out %>% dplyr::mutate(income = mapply(runif, n = 1, min = lower, max = upper))
  hp_data_out <- hp_data_out %>% dplyr::rowwise() %>% dplyr::mutate(income = replace(income,qh==1, 15000-rexp(1,1/1000)))
  hp_data_out <- hp_data_out %>% dplyr::rowwise() %>% dplyr::mutate(income = replace(income,qh==11, 261000 + rexp(1,1/40000)))
  #adjust

  hp_data_out %>% dplyr::select(-upper,-lower,-qh_imputed) %>% return()
}


#' heating_system_age
#'
#' heating system age based on survey response and mean system lifetime based on a geometric failure distribution p(1-p)^t.
#' This should be replaced by a more realistic discrete weibull distribution.
#'
#' @param q7 survey system age
#' @param tech technology
#' @param params current parameters
#'
#' @returns
#' @export
#'
#' @examples heating_system_age(6,"oil",scenario_params(sD,2015))
heating_system_age <- function(q7,tech,params){

  l <- list(1,2,c(3,5),c(6,10),c(11,20),c(21,50),c(5,50))[[q7]]
  if(length(l)==1) return(l)
  beta0 <- params[[paste(tech,"system_beta",sep="_")]]
  q_fit <- weibull_params %>% dplyr::filter(beta==beta0,lifetime==params[[paste(tech,"system_lifetime",sep="_")]]) %>% dplyr::pull(q)
  pmf_vals <- DiscreteWeibull::ddweibull(l[1]:l[2], q = q_fit, beta = beta0)
  # conditional probabilities
  cond_probs <- pmf_vals / sum(pmf_vals)
  sample(l[1]:l[2],1,prob=cond_probs)
}

#' prior_install_year
#'
#' samples the likely year of installation given that the system was replaced in failure_year
#'
#' based on the posterior probability that heating system was installed in year T1 given that it failed in year T2 using
#' a non-uniform prior that the system was installed in 2015 or before. This ensures that initial_agents() has heating systems installed before the initial year.
#'
#' @param failure_year year of system replacement
#' @param tech technology
#' @param initial_year default 2015
#' @param params scenario design
#'
#' @returns year
#' @export
#'
#' @examples  prior_install_year(2020,"gas",initial_year=2015,scenario_params(sD,2015))
prior_install_year <- function(failure_year,tech, initial_year = 2015, params){

    t_vec= 1970:2015
    a_vec <- failure_year - t_vec
    a_vec[a_vec < 1] <- NA  # age must be >= 1

    beta0 <- params[[paste(tech,"system_beta",sep="_")]]
    q_fit <- weibull_params %>% dplyr::filter(beta==beta0,lifetime==params[[paste(tech,"system_lifetime",sep="_")]]) %>% dplyr::pull(q)


    likelihood <- DiscreteWeibull::ddweibull(a_vec, q = q_fit, beta = beta0)

    uniform_prior <- rep(1,length(t_vec))

    posterior_unnorm <- likelihood * uniform_prior
    posterior <- posterior_unnorm / sum(posterior_unnorm, na.rm = TRUE)

   # data.frame(Install_Year = T1_vec, Age = a_vec, Posterior = posterior)
    sample(t_vec,1,prob=posterior)
  }


#heating_system_age <- Vectorize(heating_system_age)

#' recode_construction_year
#'
#' assign a construction year
#'
#' @param q5 survey response code for construction year. q5=7 is "don't know"
#'
#' @returns
#' @export
#'
#' @examples recode_construction_year(1)
recode_construction_year <- function(q5){
  #assume don't knows were buildt between
  l <- list(c(1750,1975),c(1976,1991),c(1992,2004),c(2005,2010),c(2011,2021),c(2021,2024),c(1750,2010))[[q5]]
  year_sample <- sample_construction_years %>% dplyr::filter(construction_year <= l[2], construction_year >= l[1]) %>% dplyr::pull(construction_year)
  sample(year_sample,1)
}


#' heating_system_efficiency
#'
#' efficiency of heating system technology as a function of installtion time.
#'
#' there was a sharp improvement in oil and gas tech with the introduction of condensing boilers
#'
#' @param tech heating technology
#' @param install_time decimal time
#'
#' @returns
#' @export
#'
#' @examples  heating_system_efficiency("heat_pump",2003)
heating_system_efficiency <- function(tech, install_time) {

  switch(tech,
         "electricity" = {
           return(tech_params[["electricity_efficiency"]])
         },

         "solid_fuel" = {
           return(tech_params[["solid_fuel_efficiency"]])
         },

         "heat_pump" = {
           values <- c(tech_params[["heat_pump_cop_2010"]],tech_params[["heat_pump_cop_2020"]])
           return(approx(x = c(2010.5, 2020.5), y = values, xout = install_time, rule = 2)$y)
         },

         "oil" = ,
         "gas" = {
           values <- c(tech_params[["boiler_efficiency_2005"]],tech_params[["boiler_efficiency_2010"]],tech_params[["boiler_efficiency_2015"]])
           return(approx(x = c(2005.5, 2010.5, 2015.5), y = values, xout = install_time, rule = 2)$y)
         },

         stop("Unknown technology type: ", tech)
  )
}


#' roundr
#'
#' stochastic round
#'
#' @param x real number to be rounded up or down
#'
#' @return integer
#' @export
#'
#' @examples replicate(1000,roundr(1.8)) %>% table()
roundr <- function(x){
  x1 <- trunc(x)
  weights = c(1+x1-x,x-x1)
  return(sample(c(x1,x1+1),size=1,prob=weights))
}

#' weibull_failure
#'
#' stochastic function that returns FALSE is the heating system did not fail during an nbservation period.
#' Use in update_agents() to model failure and replacement of heating systems according to a weibull failure model
#'
#' @param t0 installation date
#' @param t1 start of observation period
#' @param t2 end of observation period
#' @param tech heating technology
#'
#' @returns did it fail? TRUE/FALSE
#' @export
#'
#' @examples replicate(1000,weibull_failure(2010,2015,2016,"oil")) %>% table()
weibull_failure <- function(t0,t1, t2, tech) {

  t1 <- t1 - t0
  t2 <- t2 - t0

  beta <- tech_params[[paste(tech,"system_beta",sep="_")]]
  lifetime <- tech_params[[paste(tech,"system_lifetime",sep="_")]]
  q <- lifetime/gamma(1+1/beta)


  s1 <- exp(- (t1 / q)^beta)
  s2 <- exp(- (t2 / q)^beta)

  fail_prob <- (s1 - s2) / s1  # Conditional probability
  sample(c(FALSE,TRUE),1,prob=c(1-fail_prob,fail_prob))
}

weibull_failure <- Vectorize(weibull_failure)



#' expected_residual_lifetime_num
#'
#' the expected remaining lifetime of a heating system given its age
#'
#'
#' @param tech technology
#' @param system_age system age
#'
#' @returns
#' @export
#'
#' @examples expected_residual_lifetime_num("heat_pump",20)
expected_residual_lifetime_num <- function(tech, system_age) {
  beta <- tech_params[[paste(tech, "system_beta", sep = "_")]]
  lifetime <- tech_params[[paste(tech, "system_lifetime", sep = "_")]]
  q <- lifetime / gamma(1 + 1 / beta)

  f <- function(tau) exp(-((system_age + tau) / q)^beta + (system_age / q)^beta)


  integrate(f, lower = 1e-6, upper = 200)$value
}


#' expected_residual_lifetime
#'
#' expected residual lifetime of heating system contingent upon current system age
#'
#' @param tech heating tech
#' @param system_age age in years
#'
#' @returns residual lifetime in years
#' @export
#'
#' @examples expected_residual_lifetime("heat_pump",15)
expected_residual_lifetime <- function(tech,system_age) {

  beta <- tech_params[[paste(tech, "system_beta", sep = "_")]]
  lifetime <- tech_params[[paste(tech, "system_lifetime", sep = "_")]]
  lambda <- lifetime / gamma(1 + 1 / beta)

  z <- (system_age / lambda)^beta
  lambda / beta * exp(z) * gamma(1 / beta) * pgamma(z, shape = 1 / beta, lower.tail = FALSE)
}
#

#' eac_weibull
#'
#' eac_weibull calculates the unit cost expected equivalent annual cost (or capital reduction factor CRF) when the asset survival function is
#' \deqn{S(t) = \exp\Big[-\Big(\frac{t}{\lambda}\Big)^{\beta}\Big]}
#' i.e. a Weibull distribution and the continuous time discount rate is r.
#'
#'
#' @param lifetime mean system lifetime
#' @param beta weibull shape parameter (usually > 1)
#' @param system_age vector of system ages
#' @param r continuous time discount rate
#'
#' @returns vector of annualised costs assumi
#' @export
#'
#' @examples
eac_weibull <- function(lifetime, beta, system_age,r) {

  upper<- 1000
  lambda <- lifetime / gamma(1 + 1 / beta)

  # CRF for a new system
  denom0 <- integrate(function(u) exp(-r*u) * exp(-(u/lambda)^beta), 0, upper, rel.tol = 1e-8)$value
  crf <- 1 / denom0

  # Remaining PV at each age t
  #remaining <- sapply(system_age, function(tt) {
  #  denom_t <- integrate(function(u) exp(-r*u) * exp(-((tt+u)/lambda)^beta), 0, upper, rel.tol = 1e-8)$value
  #  crf * denom_t
  #})

  # Exact annualised cost per year: integrate over each year
  annualised <- sapply(system_age, function(tt) {
    integrate(function(u) exp(-r*u) * exp(-((tt+u)/lambda)^beta), 0, 1, rel.tol = 1e-8)$value* crf
  })

  return(annualised)
}

#' erv_weibull
#'
#' The expected residual value of an asset with Weibull survival function
#' \deqn{S(t) = \exp\Big[-\Big(\frac{t}{\lambda}\Big)^{\beta}\Big]}
#' conditional on its current age.
#'
#' The continuous time discount rate is related to the annual rate \eqn{\delta} by
#' \deqn{r=log(1+\delta)}
#'
#'
#' @param lifetime mean system lifetime
#' @param beta weibull shape parameter (usually > 1)
#' @param system_age vector of system ages
#' @param r discount rate
#'
#' @returns vector of ervs
#' @export
#'
#' @examples
erv_weibull <- function(lifetime, beta, system_age,r) {

  upper<- 1000
  lambda <- lifetime / gamma(1 + 1 / beta)

  # CRF for a new system
  denom0 <- integrate(function(u) exp(-r*u) * exp(-(u/lambda)^beta), 0, upper, rel.tol = 1e-8)$value
  crf <- 1 / denom0

  # Remaining PV at each age t
  remaining <- sapply(system_age, function(tt) {
    denom_t <- integrate(function(u) exp(-r*u) * exp(-((tt+u)/lambda)^beta), 0, upper, rel.tol = 1e-8)$value
    crf * denom_t
  })

  return(remaining)
}


#' space_heating_requirement
#'
#' This function calculates the annual space heating requirement from ber rating and floor area. A rebound effect is include.
#'
#' The ber rating includes contributions from hot water heating and lighting. Standard of these components are removed. For A rated
#' houses space heating energy rating is set to 15.
#'
#' @param ber quoted ber rating
#' @param floor_area floor area
#' @param rebound rebound parameter default 0.3.
#' @param params parameters
#'
#' @returns heating requirement in kWh/m2/year
#' @export
#'
#' @examples
#' params <- scenario_params(sD,2025)
#' space_heating_requirement(400,100,0,params)
#'
#' space_heating_requirement(100,200,0.5,params)
space_heating_requirement <- function(ber,floor_area,rebound=0.5,params) {

  #offset <- params$q_passive +params$q_hotwater+params$q_lighting #minimum possible
  ber_heat <- pmax(params$q_passive,ber-params$q_hotwater-params$q_lighting) #minimum heating requirement is q_passive
  #rebound effect above rebound_threshold
  ber_heat <- ifelse(ber_heat <= params$rebound_threshold, ber_heat, params$rebound_threshold + (1-rebound)*(ber_heat-params$rebound_threshold))
  ber_heat*floor_area %>% return()
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
#' @examples
#'
#' sapply(seq(50,500,by=10), function(ber) heating_system_size(ber*90))
heating_system_size <- function(annual_heating_requirement, hdd_annual=2200, coldest_day=-7,operating_hours=12, efficiency=0.9) {

  #average heating required per degree day
  kwh_per_hdd <- annual_heating_requirement / hdd_annual

  hdd_worst_day <- 16-coldest_day
  #
  kw_peak <- kwh_per_hdd * hdd_worst_day/24
  #assume system runs for 1 hours per day on coldest days
  return(ceiling(24/operating_hours*kw_peak/efficiency))
}
