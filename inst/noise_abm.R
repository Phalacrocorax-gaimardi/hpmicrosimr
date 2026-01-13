####################################
# ParBayesOptimisation
##########################################

library(hpmicrosimr)
#library(ParBayesianOptimization)  # Alternative package
library(tidyverse)

sonic <- FALSE

#limit further bias correction than already present in micro-calibration
#macro-calibration for beta.,p.,nu., rho.
#idea is that beta. is poorly determined and should be constrained by calibration
ifelse(sonic, efficiency_cal <-  readr::read_csv("/home/people/jwheatley/hpretrofit/calibration/efficiency_calibration_data.csv"),
       efficiency_cal <-  readr::read_csv("C:/Users/Joe/pkgs/hpmicrosimr/inst/efficiency_calibration_data.csv"))
efficiency_cal <- efficiency_cal %>%
  pivot_wider(names_from=measure, values_from=n) %>%
  mutate(date=lubridate::dmy(date))
names(efficiency_cal)[2:3] <- paste(names(efficiency_cal)[2:3], "_obsv", sep="")

ifelse(sonic,grants_cal <- readr::read_csv("/home/people/jwheatley/hpretrofit/calibration/grant_calibration_data.csv"),
       grants_cal <- readr::read_csv("C:/Users/Joe/pkgs/hpmicrosimr/inst/grant_calibration_data.csv"))
grants_cal <- grants_cal %>% mutate(date=lubridate::dmy(date))


data(sD)

n_run <- 4

min_fun2 <- function(nu,p,r,beta,eta,tau,lambda) {
  
  #flush.console()
  #on.exit(closeAllConnections(),add=TRUE)
  
  cal_dates <- c("2025-11-01","2021-01-01")
  out <- suppressWarnings(
    suppressMessages(
      capture.output(df <- calABM(sD, Nrun=n_run, 2, TRUE, nu=nu, p=p,r=r, beta=beta, eta=eta, tau=tau, lambda=lambda, rho=0.3))))
  
  df_eff <- df$efficiency %>% dplyr::inner_join(efficiency_cal, by="date") %>% suppressMessages()
  n_heat_pump0 <- df_eff %>% filter(date=="2015-01-01") %>% pull(n_heat_pump)
  n_b2_0 <- df_eff %>% filter(date=="2015-01-01") %>% pull(n_b2)
  df_eff <- df_eff %>% filter(date %in% cal_dates) %>%
    mutate(n_heat_pump_err = (n_heat_pump - n_heat_pump0 - n_heat_pump_obsv)/n_heat_pump_obsv,
           n_b2_err = (n_b2 - n_b2_0 - n_b2_obsv)/n_b2_obsv)  %>% suppressMessages()
  
  df_grant <- df$grants %>% dplyr::inner_join(grants_cal)  %>% suppressMessages()
  df_grant <- df_grant %>% filter(date %in% cal_dates) %>%
    mutate(n_error = (n_grant - n_obsv)/n_obsv,
           euro_error = (grants_Meuro - Meuro_obsv)/Meuro_obsv) %>% suppressMessages()
  
  err_2025 <- sum((df_grant %>% filter(date=="2025-11-01") %>% pull(n_error))^2)
  err_2025 <- err_2025 + sum((df_grant %>% filter(date=="2025-11-01") %>% pull(euro_error))^2)
  
  df_eff <- df_eff %>% filter(date=="2025-11-01")
  err_2025 <- err_2025 + sum(df_eff$n_heat_pump_err^2 + df_eff$n_b2_err^2)
  
  #print(paste("evaluated at nu. =", nu, "p. = ", p, "beta.=", beta,
  #          "r.=", r, "eta=", eta, "tau=", tau, "error=", err_2025))
  
  return(list(Score=-err_2025))
}

df <- tibble()

for(i in 1:2){
  df <- df %>% bind_rows(tibble(run=i,objective=min_fun2(0.6,0.04,0.03,0.6,0.2,0.02,0)))
}


# Save results
if(sonic) write_csv(df, "/home/people/jwheatley/hpretrofit/calibration/noise_abm_16.csv")
#if(!sonic) write_csv(calib, "C:/Users/Joe/pkgs/hpmicrosimr/inst/macro_calibration_parbayes_test.csv")

cat("\nResults saved to noise_abm_16.csv\n")
