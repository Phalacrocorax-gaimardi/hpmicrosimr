library(hpmicrosimr)
library(tidyverse)

abm_cal <- calABM2(sD, Nrun = 4, n_unused_cores = 2, use_parallel = TRUE, quiet = FALSE )
hpmicrosimr:::cal_error(abm_cal)
abm_cal$efficiency
abm_cal$grants

abm <- runABM(sD, 8, 2025, use_parallel = TRUE)
n_sim <- dplyr::n_distinct(abm[[1]]$simulation)
abm[[1]] %>%
  dplyr::arrange(simulation, serial, date) %>%
  dplyr::group_by(simulation, serial) %>%
  dplyr::mutate(ber_prev = dplyr::lag(ber, default = Inf)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(grant_type), grant_type != "None", date <= as.Date("2025-11-01"),
                (upgrade_grant > 0 | heat_pump_grant > 0), ber < 125) %>%
  dplyr::count(grant_type, hp = (tech == "heat_pump"), crossed = (ber_prev >= 125)) %>%
  dplyr::mutate(households = n * 1451 / n_sim)

abm[[1]] %>%
  dplyr::filter(grant_type == "BetterEnergyHomes", upgrade_grant > 0, tech != "heat_pump",
                date <= as.Date("2025-11-01")) %>%
  dplyr::summarise(
    n = dplyr::n(),
    hli_q = list(round(quantile(hli, c(.1, .25, .5, .75, .9)), 2)),
    grant_q = list(round(quantile(upgrade_grant, c(.1, .25, .5, .75, .9)))),
    share_hli_lt_1.4 = mean(hli < 1.4),
    share_hli_floor  = mean(hli < 0.3)
  )

##############################
# limiting and diagnostic cases
##############################

#sD$value[sD$parameter == "warmer_homes_target_ber"] <- 120
#sD$value[sD$parameter == "warmer_homes_heat_pump"] <- 2015   # print this year, I don't know its value
test <- calABM2(sD, 1, use_parallel = FALSE, nu = 100, p = 0.0569, beta = 0.57, r = 0.04, eta = 0.103, tau = 0.029)
test$grants


#full rational decision making (nu -> large implies financial dominates)
#failure & heat pump economics driven
test <- calABM2(sD,1,use_parallel=F,nu=0,p=0.,beta=1,r=0.05,eta=0.,tau=0.0,)
cal_error(test)
test$efficiency
#failure & environmental views driven
test <- calABM2(sD,1,use_parallel=F,nu=0,p=0.01,beta=1,r=0.05,eta=0.,tau=0.0)
cal_error(test)
test <- calABM2(sD,1,use_parallel=F,nu=100,p=0.0,beta=1,r=0.05,eta=0.,tau=0.0)
cal_error(test)
#only behavioural factors in heating system failures - no upgrades
test <- calABM2(sD,1,use_parallel=F,nu=0,p=0.,beta=1,r=0.05,eta=0.,tau=0.0)
cal_error(test)

#scans - nu dependence of heat pump uptake from failures
scan <- tibble()
for(p in 0)
for(nu in seq(0,1,by=0.1)) {
  print(paste("nu=",nu,"p=",p))
  res <- calABM2(sD,1,use_parallel=F,nu=nu,p=p,beta=1,r=0.05,eta=0.05,tau=0.0)
  calerror <- cal_error(res)
  sc <- res$parameters
  sc$error <- calerror
  scan <- scan %>% bind_rows(sc)
}
#write_csv(scan,"~/Policy/CAMG/EED/Heat/data/new_calibrations/scans/nu_p_scan.csv")
#scan <- read_csv("~/Policy/CAMG/EED/Heat/data/new_calibrations/scans/nu_p_scan.csv")
scan %>% ggplot(aes(nu.,n_heat_pump_grant,colour=factor(p.)))+geom_line()+geom_hline(yintercept=18300) + geom_point()
scan %>% ggplot(aes(nu.,error,colour=factor(p.)))+geom_line()

#no behavioural barriers, just p.
scan <- tibble()
for(p in seq(0.0,0.04,by=0.01))
  for(nu in 100) {
    print(paste("nu=",nu,"p=",p))
    res <- calABM2(sD,1,use_parallel=F,nu=nu,p=p,beta=1,r=0.05,eta=0.1,tau=0.0)
    calerror <- cal_error(res)
    sc <- res$parameters
    sc$error <- calerror
    scan <- scan %>% bind_rows(sc)
  }
#write_csv(scan,"~/Policy/CAMG/EED/Heat/data/new_calibrations/scans/nu_p_scan_eta0.1.csv")


grid_scan <- read_csv("~/Policy/CAMG/EED/Heat/data/new_calibrations/scans/grid_scan.csv")

grid_scan <- grid_scan %>% mutate(error= (n_heat_pump_grant/18233-1)^2 + (n_b2_grants/83800-1)^2)

grid_scan %>% ggplot(aes(nu.,n_b2_grants/n_heat_pump_grant,colour==p.)) + geom_line() + facet_wrap(.~eta.)

#increasing upgrade decision rate
scan <- tibble()
for(p in seq(0,0.05,by=0.01)) {
  print(nu)
  scan <- scan %>% bind_rows(calABM2(sD,1,use_parallel=F,nu=0.35,p=p,beta=1,r=0.05,eta=0.,tau=0.0)$parameters)
}
write_csv(scan,"~/Policy/CAMG/EED/Heat/data/new_calibrations/scans/p_scan_no_upgrades.csv")

#increasing eta
scan <- tibble()
for(eta in seq(0,0.2,by=0.02)) {
  print(nu)
  scan <- scan %>% bind_rows(calABM2(sD,1,use_parallel=F,nu=0.35,p=0.04,beta=1,r=0.05,eta=eta,tau=0.0)$parameters)
}


