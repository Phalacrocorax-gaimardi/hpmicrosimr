# Bayesian optimisation of the hpmicrosimr calibration error with mlr3mbo
#
# UNTESTED: written without access to R. Run the pre-flight block first.
#
# Needs: hpmicrosimr (>= 0.4.01), bbotk, paradox, mlr3, mlr3learners, mlr3mbo,
#        DiceKriging (surrogate), lhs (initial design), data.table
#
# Design choices
#  * 5 free parameters: p., r., beta., eta., tau.  nu. and rho. are fixed at their sD values.
#  * The objective is log(cal_error): errors are positive, span orders of magnitude, and a
#    Gaussian process fits the log far better.
#  * The simulation is stochastic (32 replicates), so the GP has an estimated nugget (noise term).
#  * Every evaluation is appended to a CSV so a crashed overnight run loses nothing.
#  * The best *observed* point under noise is optimistic (winner's curse), so the best few
#    candidates are re-run with many more replicates at the end.

library(hpmicrosimr)
library(data.table)
library(paradox)
library(bbotk)
library(mlr3)
library(mlr3learners)
library(mlr3mbo)

## ---- settings -------------------------------------------------------------------------
n_rep           <- 32      # replicates per evaluation
n_unused_cores  <- 2
n_init          <- 20      # initial space-filling design (about 4 x number of parameters)
n_evals         <- 120     # total evaluations including the initial design
n_confirm       <- 5       # best candidates to re-run
n_rep_confirm   <- 128     # replicates for the confirmation runs
penalty         <- log(100)  # log-error returned if a simulation fails (set above your worst real error)
log_file        <- "mbo_calibration_log.csv"

# fixed parameters taken from the scenario dataframe
sD_value <- function(name) sD$value[sD$parameter == name]
nu_fixed  <- sD_value("nu.")
rho_fixed <- sD_value("rho.")

# search space for the five free parameters: EDIT THESE BOUNDS
domain <- ps(
  p    = p_dbl(0.005, 0.1),
  r    = p_dbl(0.01,  0.10),
  beta = p_dbl(0.30,  1.00),
  eta  = p_dbl(0.02,  0.30),
  tau  = p_dbl(0.005, 0.10)
)

# current sD values, included as one point of the initial design
start <- data.table(
  p    = sD_value("p."),
  r    = sD_value("r."),
  beta = sD_value("beta."),
  eta  = sD_value("eta."),
  tau  = sD_value("tau.")
)

## ---- one simulation-based evaluation --------------------------------------------------
run_cal <- function(p, r, beta, eta, tau, n_rep) {
  abm_cal <- calABM2(sD, Nrun = n_rep, n_unused_cores = n_unused_cores, use_parallel = TRUE,
                     nu = nu_fixed, p = p, beta = beta, r = r, eta = eta, tau = tau,
                     rho = rho_fixed, quiet = TRUE)
  list(error  = hpmicrosimr:::cal_error(abm_cal),   # cal_error is not exported
       n_heat = abm_cal$parameters$n_heat,
       n_b2   = abm_cal$parameters$number_b2)
}

## ---- pre-flight check: one evaluation at the current sD values ---------------------------------
t0 <- Sys.time()
pre <- do.call(run_cal, c(as.list(start), n_rep = 2))   # 2 replicates, just to check it runs
stopifnot(is.finite(pre$error))
cat("pre-flight OK: error =", pre$error, " (", format(Sys.time() - t0), "for 2 replicates)\n")

## ---- objective ------------------------------------------------------------------------
objective <- ObjectiveRFun$new(
  fun = function(xs) {
    t0  <- Sys.time()
    res <- tryCatch(
      do.call(run_cal, c(xs[c("p", "r", "beta", "eta", "tau")], n_rep = n_rep)),
      error = function(e) {
        message("evaluation failed: ", conditionMessage(e))
        list(error = exp(penalty), n_heat = NA_real_, n_b2 = NA_real_)
      })
    secs <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    fwrite(data.table(as.data.table(xs[c("p", "r", "beta", "eta", "tau")]),
                      error = res$error, log_error = log(res$error),
                      n_heat = res$n_heat, n_b2 = res$n_b2, n_rep = n_rep, secs = secs),
           log_file, append = file.exists(log_file))
    # extra values (error, n_heat, n_b2) are stored in the archive alongside the codomain
    list(log_error = log(res$error), error = res$error, n_heat = res$n_heat, n_b2 = res$n_b2)
  },
  domain   = domain,
  codomain = ps(log_error = p_dbl(tags = "minimize")),
  id       = "hpmicrosimr_cal"
)

instance <- oi(objective, terminator = trm("evals", n_evals = n_evals))

## ---- initial design -------------------------------------------------------------------
set.seed(1)
design <- generate_design_lhs(instance$search_space, n_init - 1)$data
design <- rbind(start, design)
instance$eval_batch(design)

## ---- surrogate, acquisition, optimiser ------------------------------------------------
learner <- lrn("regr.km",
               covtype = "matern5_2",
               optim.method = "gen",
               nugget.estim = TRUE,                # simulation noise
               control = list(trace = FALSE))
surrogate     <- srlrn(learner, catch_errors = TRUE)
acq_function  <- acqf("ei")
acq_optimizer <- acqo(opt("random_search", batch_size = 2000),
                      terminator = trm("evals", n_evals = 2000))

optimizer <- opt("mbo",
                 loop_function = bayesopt_ego,
                 surrogate     = surrogate,
                 acq_function  = acq_function,
                 acq_optimizer = acq_optimizer)

optimizer$optimize(instance)
saveRDS(instance, "mbo_instance.rds")

## ---- confirmation of the best candidates ----------------------------------------------
arch <- as.data.table(instance$archive)
best <- head(arch[order(log_error)], n_confirm)
cat("\nBest observed (", n_rep, "replicates each):\n")
print(best[, .(p, r, beta, eta, tau, error, n_heat, n_b2)])

confirm <- rbindlist(lapply(seq_len(nrow(best)), function(i) {
  res <- do.call(run_cal, c(as.list(best[i, .(p, r, beta, eta, tau)]), n_rep = n_rep_confirm))
  cbind(best[i, .(p, r, beta, eta, tau)], error_confirm = res$error,
        n_heat = res$n_heat, n_b2 = res$n_b2)
}))
cat("\nRe-run with", n_rep_confirm, "replicates:\n")
print(confirm[order(error_confirm)])
fwrite(confirm, "mbo_calibration_confirmed.csv")
