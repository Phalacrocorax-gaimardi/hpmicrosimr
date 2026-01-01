####################################
# ParBayesOptimisation
##########################################

library(hpmicrosimr)
library(ParBayesianOptimization)  # Alternative package
library(tidyverse)

sonic <- FALSE
n_design <- 20
n_iter <- 20
n_run <- 4

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

min_fun2 <- function(nu,p,r,beta,eta,tau) {

  #flush.console()
  #on.exit(closeAllConnections(),add=TRUE)

  cal_dates <- c("2025-11-01","2021-01-01")
  out <- suppressWarnings(
        suppressMessages(
          capture.output(df <- calABM(sD, Nrun=n_run, 2, TRUE, nu=nu, p=p,r=r, beta=beta, eta=eta, tau=tau, rho=0.3))))

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
  err_2025 <- err_2025 + 3*sum(df_eff$n_heat_pump_err^2 + df_eff$n_b2_err^2)

  #print(paste("evaluated at nu. =", nu, "p. = ", p, "beta.=", beta,
  #          "r.=", r, "eta=", eta, "tau=", tau, "error=", err_2025))

  return(list(Score=-err_2025))
}

params  <- scenario_params(sD,2015)
min_fun2(params$nu.,params$p.,params$r.,params$beta.,params$eta.,params$tau.)

# Set bounds
bounds <- list(
  nu = c(0.1, 0.7),
  p = c(0.002, 0.02),
  beta = c(0.3, 1),
  r = c(0.01, 0.1),
  eta = c(0, 0.1),
  tau = c(0, 0.1)
)

# Run optimization

foreach::registerDoSEQ()

result <- bayesOpt(
  FUN = min_fun2,
  bounds = bounds,
  initPoints = n_design,
  iters.n = n_iter,
  iters.k= 1,
  parallel = FALSE  # Force sequential
)

# Extract all results
calib <- results$scoreSummary %>% tibble::as_tibble() %>% dplyr::select(Epoch,Iteration,nu,p,r,beta,eta,tau,Score) %>% dplyr::mutate(error=-Score)
calib <- calib %>% arrange(error) %>% select(-Score)

# Save results
if(sonic) write_csv(calib, "/home/people/jwheatley/hpretrofit/calibration/macro_calibration_parbayes_test.csv")
if(!sonic) write_csv(calib, "C:/Users/Joe/pkgs/hpmicrosimr/inst/macro_calibration_parbayes_test.csv")

cat("\nResults saved to macro_calibration_parbayes.csv\n")
