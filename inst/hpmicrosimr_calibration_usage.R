#########################
#calibration package usage
###########################
library(hpmicrosimr)
library(mlrMBO)
library(ParamHelpers)
library(smoof)
#library(DiceKriging)
library(tidyverse)
#library(rgenoud)

n_design <- 30
n_iter <- 30
n_run <- 4

sD <- readxl::read_xlsx("C:/Users/Joe/pkgs/hpmicrosimr/inst/extdata/scenario_parameters.xlsx",sheet="WEM")

#limit further bias correction than already present in micro-calibration
#macro-calibration for beta.,p.,nu., rho.
#idea is that beta. is poorly determined and should be constrained by calibration
efficiency_cal <- readr::read_csv("C:/Users/Joe/pkgs/hpmicrosimr/inst/efficiency_calibration_data.csv")
efficiency_cal <- efficiency_cal %>% pivot_wider(names_from=measure,values_from=n) %>% mutate(date=dmy(date))
names(efficiency_cal)[2:3] <- paste(names(efficiency_cal)[2:3],"_obsv",sep="")

grants_cal <- readr::read_csv("C:/Users/Joe/pkgs/hpmicrosimr/inst/grant_calibration_data.csv")
grants_cal <-grants_cal %>% mutate(date=dmy(date))

# x <- c(0.4,0.006,0.8,0.04,0.02,0.02)
min_fun2 <- function(x){
  #
  cal_dates <- c("2025-11-01","2021-01-01")
  #x[1] beta. x[2] lambda. x[3] p.
  #df <- calABM(sD,Nrun=8,beta=x[1],lambda=0,p=x[2],nu=x[3],rho=x[4],delta=x[5])
  df <- calABM(sD,n_run,2,TRUE,nu=x[1],p=x[2],beta = x[3],r = x[4],eta = x[5],tau = x[6],rho=0.3)

  df_eff <- df$efficiency %>% dplyr::inner_join(efficiency_cal,by="date")
  n_heat_pump0 <- df_eff %>% filter(date=="2015-01-01") %>% pull(n_heat_pump)
  n_b2_0 <- df_eff %>% filter(date=="2015-01-01") %>% pull(n_b2)
  df_eff <- df_eff %>% filter(date %in% cal_dates) %>% mutate(n_heat_pump_err= (n_heat_pump-n_heat_pump0-n_heat_pump_obsv)/n_heat_pump_obsv, n_b2_err = (n_b2-n_b2_0-n_b2_obsv)/n_b2_obsv)

  df_grant <- df$grants %>% dplyr::inner_join(grants_cal)
  #minimise the % error in grant totals
  df_grant <- df_grant %>% filter(date %in% cal_dates) %>% mutate(n_error = (n_grant-n_obsv)/n_obsv, euro_error = (grants_Meuro-Meuro_obsv)/Meuro_obsv)
  err_2025  <- sum((df_grant %>% filter(date=="2025-11-01") %>% pull(n_error))^2)
  err_2025 <- err_2025 + sum((df_grant %>% filter(date=="2025-11-01") %>% pull(euro_error))^2)
  df_eff <- df_eff %>% filter(date=="2025-11-01")
  err_2025 <- err_2025 + 3*sum(df_eff$n_heat_pump_err^2 + df_eff$n_b2_err^2 )
  print(paste("evaluated at nu. =",x[1],"p. = ",x[2],"beta.=",x[3],"r.=",x[4],"eta=",x[5],"tau=",x[6],"error=", err_2025))
  err_2025 %>% return()
}


search_space <- makeParamSet(
  makeNumericParam("nu.",0.2,0.8),
  makeNumericParam("p.",lower=0.002,upper=0.02),
  makeNumericParam("beta.",lower=0.4,upper=1),
  makeNumericParam("r.",0.01,0.15),
  makeNumericParam("eta.",0,0.05), #disruption
  makeNumericParam("tau.",0,0.05) #sludge
  #discount rates from 2% up to 20%
)

obj_fun <- makeSingleObjectiveFunction(
  name="efficiency Calibrate",
  fn=min_fun2,
  par.set=search_space,
  minimize=TRUE
)

ctrl <- makeMBOControl(
  propose.points=1,
  final.method="best.predicted"
)

ctrl <- setMBOControlTermination(ctrl,
                                 iters=n_iter)

infill_crit <- makeMBOInfillCritEI()

ctrl <- setMBOControlInfill(ctrl,
                            crit=infill_crit ) #expected improvement
#Latin Hypercube Design
initial_design <- generateDesign(
  n=n_design,
  par.set=search_space,
  fun=lhs::randomLHS
)

result <- mbo(
  fun=obj_fun,
  design=initial_design,
  control=ctrl
)

macro_calibration <- result$opt.path %>% as_tibble() %>% arrange(y)
macro_calibration <- macro_calibration %>% select(nu.,p.,beta.,r.,eta.,tau.,y)
write_csv(macro_calibration,paste("C:/Users/Joe/pkgs/hpmicrosimr/inst/macro_calibration_2.csv",sep=""))
#write_csv(macro_calibration,paste("~/Policy/CAMG/SolarPVReport/PVBESS_microsimr/macro_calibration_5_parameter_50.csv",sep=""))

macro_calibration <- read_csv("C:/Users/Joe/pkgs/hpmicrosimr/inst/macro_calibration_2.csv")
mc <- macro_calibration[1,]
min_fun2(c(nu=mc$nu.,p=mc$p.,beta=mc$beta.,r=mc$r.,eta=mc$eta.,tau=mc$tau.))
test <- calABM(sD,4,T,nu=mc$nu.,p=mc$p.,r=mc$r.,beta=mc$beta.,eta=mc$eta.,tau=mc$tau.)


######################
# continue iterating
####################

add_iters <- 10

initial_data <- as.data.frame(result$opt.path)[,c("beta.","p.","nu.","rho.","delta.")]

ctrl_add <- makeMBOControl(
  propose.points=1,
  final.method="best.predicted"
)

ctrl_add <- setMBOControlTermination(ctrl,
                                 iters=add_iters)


#ctrl <- makeMBOControl(iters=add_iters) #adds 25 more interations

result_add <- mbo(fun=obj_fun, design=initial_data, control=ctrl_add)

macro_calibration_add_1 <- result_add$opt.path %>% as_tibble() %>% arrange(y)
macro_calibration_add_1 <- macro_calibration_add_1 %>% mutate(lambda.=0)
macro_calibration_add_1 <- macro_calibration_add_1 %>% select(beta.,lambda.,p.,nu.,rho.,delta.,,y)



############################
#
################################
min_fun_check <- function(calABM_out,x){
  #
  cal_dates <- c("2025-11-01","2021-01-01")
  #x[1] beta. x[2] lambda. x[3] p.
  #df <- calABM(sD,Nrun=8,beta=x[1],lambda=0,p=x[2],nu=x[3],rho=x[4],delta=x[5])
  df <- calABM(sD,n_run,2,TRUE,nu=x[1],p=x[2],beta = x[3],r = x[4],eta = x[5],tau = x[6],rho=0.3)

  df_eff <- df$efficiency %>% dplyr::inner_join(efficiency_cal,by="date")
  n_heat_pump0 <- df_eff %>% filter(date=="2015-01-01") %>% pull(n_heat_pump)
  n_b2_0 <- df_eff %>% filter(date=="2015-01-01") %>% pull(n_b2)
  df_eff <- df_eff %>% filter(date %in% cal_dates) %>% mutate(n_heat_pump_err= (n_heat_pump-n_heat_pump0-n_heat_pump_obsv)/n_heat_pump_obsv, n_b2_err = (n_b2-n_b2_0-n_b2_obsv)/n_b2_obsv)

  df_grant <- df$grants %>% dplyr::inner_join(grants_cal)
  #minimise the % error in grant totals
  df_grant <- df_grant %>% filter(date %in% cal_dates) %>% mutate(n_error = (n_grant-n_obsv)/n_obsv, euro_error = (grants_Meuro-Meuro_obsv)/Meuro_obsv)
  err_2025  <- sum((df_grant %>% filter(date=="2025-11-01") %>% pull(n_error))^2)
  err_2025 <- err_2025 + sum((df_grant %>% filter(date=="2025-11-01") %>% pull(euro_error))^2)
  df_eff <- df_eff %>% filter(date=="2025-11-01")
  err_2025 <- err_2025 + 3*sum(df_eff$n_heat_pump_err^2 + df_eff$n_b2_err^2 )
  print(paste("evaluated at nu. =",x[1],"p. = ",x[2],"beta.=",x[3],"r.=",x[4],"eta=",x[5],"tau=",x[6],"error=", err_2025))
  err_2025 %>% return()
}


sD <- readxl::read_xlsx("~/Policy/CAMG/SolarPVReport/PVBESS_microsimr/scenario_parameters.xlsx", sheet="scenario_BASE")
params <- scenario_params(sD,2020)
test <- calABM(sD,16,beta=params$beta.,lambda=0,p=params$p.,nu=params$nu.,rho=params$rho.,delta=params$delta.)
test %>% inner_join(pv_retrofit) %>% ggplot(aes(date,n))+geom_point()+ geom_point(aes(date,total),colour="red")
test %>% inner_join(pv_retrofit) %>% ggplot(aes(date,MW))+geom_point()+ geom_point(aes(date,MW_obs),colour="red")

test <- calABM(sD,10,beta=0.27033796,lambda=0,p=0.008068522,nu=0.733091826,rho=0.382862221,delta=0.037169339)
test_1 <- calABM(sD,10,beta=0.213,lambda=0,p=0.01,nu=0.609,rho=0.505,delta=0.047)
test_1 %>% inner_join(pv_retrofit) %>% ggplot(aes(date,n))+geom_point()+ geom_point(aes(date,total),colour="red")
test_1 %>% inner_join(pv_retrofit) %>% ggplot(aes(date,MW))+geom_point()+ geom_point(aes(date,MW_obs),colour="red")


test4 <- calABM(sD,4,beta=params$beta.,lambda=0,p=params$p.,nu=params$nu.,rho=params$rho.,delta=params$delta.)
test3 <- calABM(sD,32,beta=params$beta.,lambda=0,p=params$p.,nu=params$nu.,rho=params$rho.,delta=params$delta.)



