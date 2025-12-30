#########################
#calibration package usage - FIXED VERSION
###########################
# ===== CRITICAL: DISABLE ALL PARALLEL IN mlrMBO =====
Sys.setenv(R_ENABLE_JIT="0", OMP_NUM_THREADS="1", 
           OPENBLAS_NUM_THREADS="1", MKL_NUM_THREADS="1")

# Set BEFORE loading mlrMBO
options(mc.cores = 1, Ncpus = 1, mlrMBO.parallel = FALSE)

library(hpmicrosimr)
library(mlrMBO)
library(tidyverse)

# Load data
data(sD)

# ===== YOUR ORIGINAL min_fun2 (unchanged) =====
min_fun2 <- function(x) {
  cal_dates <- c("2025-11-01","2021-01-01")
  df <- calABM(sD, Nrun=32, 2, TRUE, nu=x[1], p=x[2], beta=x[3], 
               r=x[4], eta=x[5], tau=x[6], rho=0.3)
  
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
  
  print(paste("evaluated at nu. =", x[1], "p. = ", x[2], "beta.=", x[3],
              "r.=", x[4], "eta=", x[5], "tau=", x[6], "error=", err_2025))
  
  return(err_2025)
}

# Load calibration data
efficiency_cal <- readr::read_csv("/home/people/jwheatley/hpretrofit/calibration/efficiency_calibration_data.csv")
efficiency_cal <- efficiency_cal %>% 
  pivot_wider(names_from=measure, values_from=n) %>% 
  mutate(date=lubridate::dmy(date))
names(efficiency_cal)[2:3] <- paste(names(efficiency_cal)[2:3], "_obsv", sep="")

grants_cal <- readr::read_csv("/home/people/jwheatley/hpretrofit/calibration/grant_calibration_data.csv")
grants_cal <- grants_cal %>% mutate(date=lubridate::dmy(date))

# ===== CRITICAL: CONFIGURE mlrMBO FOR SEQUENTIAL EXECUTION =====
search_space <- makeParamSet(
  makeNumericParam("nu.", 0.1, 1.0),
  makeNumericParam("p.", lower=0.002, upper=0.012),
  makeNumericParam("beta.", lower=0.4, upper=1),
  makeNumericParam("r.", 0.01, 0.07),
  makeNumericParam("eta.", 0, 0.05),
  makeNumericParam("tau.", 0, 0.05)
)

obj_fun <- makeSingleObjectiveFunction(
  name="efficiency Calibrate",
  fn=min_fun2,
  par.set=search_space,
  minimize=TRUE
)

# ===== FORCE SEQUENTIAL CONTROL =====
ctrl <- makeMBOControl(propose.points=1, final.method="best.predicted")

# CRITICAL: Explicitly disable parallel features
ctrl$multipoint.method <- "moimbo"  # Use sequential method
ctrl$multicrit.method <- "mspot"    # Sequential multi-crit
ctrl$y.name <- "y"

ctrl <- setMBOControlTermination(ctrl, iters=n_iters)

# Use simple infill without parallel options
infill_crit <- makeMBOInfillCritEI()
ctrl <- setMBOControlInfill(ctrl, crit=infill_crit, 
                            opt="focussearch",  # Sequential optimizer
                            opt.focussearch.points=100,
                            opt.focussearch.maxit=3)

# ===== TEST: VERIFY SEQUENTIAL EXECUTION =====
cat("Testing with 1 iteration first...\n")
ctrl_test <- ctrl
ctrl_test <- setMBOControlTermination(ctrl_test, iters=1)

initial_design <- generateDesign(
  n=n_design,
  par.set=search_space,
  fun=lhs::randomLHS
)

# ===== RUN WITH SAFETY WRAPPER =====
tryCatch({
  result <- mbo(
    fun=obj_fun,
    design=initial_design,
    control=ctrl_test,
    show.info=TRUE
  )
  cat("✓ Test successful! Running full optimization...\n")
  
  # Now run full optimization
  ctrl <- setMBOControlTermination(ctrl, iters=n_iters)
  result <- mbo(
    fun=obj_fun,
    design=initial_design,
    control=ctrl,
    show.info=TRUE
  )
  
  # Save results
  macro_calibration <- result$opt.path %>% as_tibble() %>% arrange(y)
  macro_calibration <- macro_calibration %>% select(nu., p., beta., r., eta., tau., y)
  write_csv(macro_calibration, "/home/people/jwheatley/hpretrofit/calibration/macro_calibration_1.csv")
  
}, error = function(e) {
  cat("Error:", e$message, "\n")
  
  # Try alternative: Even more restrictive settings
  cat("Trying alternative configuration...\n")
  
  # Force single-threaded everything
  library(parallelMap)
  parallelStart(mode="local", cpus=1, level="mlrMBO.feval")
  
  result <- mbo(
    fun=obj_fun,
    design=initial_design,
    control=ctrl_test,
    show.info=TRUE
  )
})