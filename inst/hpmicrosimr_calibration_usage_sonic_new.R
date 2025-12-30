
closeAllConnections()
while(sink.number() > 0) sink()

Sys.setenv(R_ENABLE_JIT="0", OMP_NUM_THREADS="1")
library(hpmicrosimr)
library(ParBayesianOptimization)  # Alternative package
library(tidyverse)

n_design <- 10
n_iter <- 5
n_run <- 16

#limit further bias correction than already present in micro-calibration
#macro-calibration for beta.,p.,nu., rho.
#idea is that beta. is poorly determined and should be constrained by calibration
efficiency_cal <- readr::read_csv("/home/people/jwheatley/hpretrofit/calibration/efficiency_calibration_data.csv")
efficiency_cal <- efficiency_cal %>% pivot_wider(names_from=measure,values_from=n) %>% mutate(date=dmy(date))
names(efficiency_cal)[2:3] <- paste(names(efficiency_cal)[2:3],"_obsv",sep="")

grants_cal <- readr::read_csv("/home/people/jwheatley/hpretrofit/calibration/grant_calibration_data.csv")
grants_cal <-grants_cal %>% mutate(date=dmy(date))


data(sD)

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


# Set bounds
bounds <- list(
  nu = c(0.1, 1.0),
  p = c(0.002, 0.012),
  beta = c(0.4, 1),
  r = c(0.01, 0.08),
  eta = c(0, 0.05),
  tau = c(0, 0.05)
)

# Run optimization


# Add sink management around bayesOpt
run_optimization <- function() {
  # Create a clean output file
  log_file <- file("optimization_log.txt", open = "wt")
  sink(log_file, type = "output", split = TRUE)  # split keeps console output
  sink(log_file, type = "message")
  
  on.exit({
    sink(type = "message")
    sink(type = "output")
    close(log_file)
  })
  
  # Run optimization
  results <- bayesOpt(
    FUN = min_fun2,
    bounds = bounds,
    initPoints = n_design,
    iters.n = n_iter,  # Start small for testing
    parallel = FALSE,
    acq = "ei",
   # gsPoints = 10,  # Reduced
    verbose = 1,
    plotProgress = FALSE
  )
  
  return(results)
}

# Run with protection
results <- tryCatch(
  run_optimization(),
  error = function(e) {
    cat("Optimization failed:", e$message, "\n")
    return(NULL)
  }
)


opt_result <- bayesOpt(
  FUN = min_fun2,
  bounds = bounds,
  initPoints = n_design,
  iters.n = n_iter,
  parallel = FALSE  # Force sequential
)

best_params <- results$scoreSummary[which.max(results$scoreSummary$Score), ]
cat("\n=== BEST RESULTS ===\n")
print(best_params)

# Extract all results
all_results <- results$scoreSummary %>%
  arrange(desc(Score)) %>%
  select(nu., p., beta., r., eta., tau., Score) %>%
  mutate(Error = -Score)  # Convert back to error

# Save results
write_csv(all_results, "/home/people/jwheatley/hpretrofit/calibration/macro_calibration_parbayes.csv")

cat("\nResults saved to macro_calibration_parbayes.csv\n")