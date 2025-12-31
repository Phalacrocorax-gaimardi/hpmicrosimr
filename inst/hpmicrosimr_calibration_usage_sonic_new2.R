
# Load packages
library(rBayesianOptimization)
library(hpmicrosimr)
library(tidyverse)
library(doParallel)

# Load data
data(sD)

#
n_design <- 8
n_iters <- 4
n_run <- 4

# Load calibration data (your existing code)
#efficiency_cal <- readr::read_csv("/home/people/jwheatley/hpretrofit/calibration/efficiency_calibration_data.csv")
efficiency_cal <- readr::read_csv("C:/Users/Joe/pkgs/hpmicrosimr/inst/efficiency_calibration_data.csv")


efficiency_cal <- efficiency_cal %>% 
  pivot_wider(names_from=measure, values_from=n) %>% 
  mutate(date=lubridate::dmy(date))
names(efficiency_cal)[2:3] <- paste(names(efficiency_cal)[2:3], "_obsv", sep="")

grants_cal <- readr::read_csv("C:/Users/Joe/pkgs/hpmicrosimr/inst/grant_calibration_data.csv")
grants_cal <- grants_cal %>% mutate(date=lubridate::dmy(date))

# Your original objective function
min_fun2 <- function(nu,p,r,beta,eta,tau) {
  
  flush.console()
  on.exit(closeAllConnections(),add=TRUE)
  
  cal_dates <- c("2025-11-01","2021-01-01")
  df <- calABM(sD, Nrun=n_run, 2, TRUE, nu=nu, p=p,r=r, beta=beta, eta=eta, tau=tau, rho=0.3)
  
  df_eff <- df$efficiency %>% dplyr::inner_join(efficiency_cal, by="date")
  n_heat_pump0 <- df_eff %>% filter(date=="2015-01-01") %>% pull(n_heat_pump)
  n_b2_0 <- df_eff %>% filter(date=="2015-01-01") %>% pull(n_b2)
  df_eff <- df_eff %>% filter(date %in% cal_dates) %>% 
    mutate(n_heat_pump_err = (n_heat_pump - n_heat_pump0 - n_heat_pump_obsv)/n_heat_pump_obsv,
           n_b2_err = (n_b2 - n_b2_0 - n_b2_obsv)/n_b2_obsv)
  
  df_grant <- df$grants %>% dplyr::inner_join(grants_cal)
  df_grant <- df_grant %>% filter(date %in% cal_dates) %>% 
    mutate(n_error = (n_grant - n_obsv)/n_obsv,
           euro_error = (grants_Meuro - Meuro_obsv)/Meuro_obsv)
  
  err_2025 <- sum((df_grant %>% filter(date=="2025-11-01") %>% pull(n_error))^2)
  err_2025 <- err_2025 + sum((df_grant %>% filter(date=="2025-11-01") %>% pull(euro_error))^2)
  
  df_eff <- df_eff %>% filter(date=="2025-11-01")
  err_2025 <- err_2025 + 3*sum(df_eff$n_heat_pump_err^2 + df_eff$n_b2_err^2)
  
  print(paste("evaluated at nu. =", nu, "p. = ", p, "beta.=", beta,
              "r.=", r, "eta=", eta, "tau=", tau, "error=", err_2025))
  
  return(list(Score=-err_2025))
}


# Bounds
bounds <- list(
  nu = c(0.1, 1.0),
  p = c(0.002, 0.012),
  beta = c(0.4, 1.0),
  r = c(0.01, 0.07),
  eta = c(0, 0.05),
  tau = c(0, 0.05)
)

# Run optimization
cat("\n=== Starting Bayesian Optimization at", Sys.time(), "===\n")
cat("Initial points: 20\n")
cat("Iterations: 30\n")
cat("Total evaluations: 50\n")

result <- BayesianOptimization(
  FUN = min_fun2,
  bounds = bounds,
  init_points = n_design,
  n_iter = n_iters,
  acq = "ei",
  kappa = 2.576,
  eps = 0.0,
  verbose = TRUE
)

# Save results
cat("\n=== Optimization complete at", Sys.time(), "===\n")
cat("Best parameters:\n")
print(result$Best_Par)
cat("Best score:", result$Best_Value, "\n")
cat("Best error:", -result$Best_Value, "\n")

#saveRDS(result, "bayesian_optimization_results.rds")

# Save all evaluations
all_evals <- data.frame(
  iteration = 1:length(result$History$Value),
  score = result$History$Value,
  error = -result$History$Value,
  result$History$Params
)

write_csv(all_evals, "C:/Users/Joe/pkgs/hpmicrosimr/inst/macro_calibration_test.csv")

