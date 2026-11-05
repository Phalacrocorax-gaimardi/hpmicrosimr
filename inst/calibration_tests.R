library(hpmicrosimr)
library(tidyverse)

##############################
# limiting and diagnostic cases
##############################

#full rational decision making (nu -> large implies financial dominates)

test <- calABM2(sD,1,use_parallel=F,nu=10,p=0.02,beta=1,r=0.05,eta=0.,tau=0.0)
cal_error(test)
test <- calABM2(sD,1,use_parallel=F,nu=10,p=0.01,beta=1,r=0.05,eta=0.,tau=0.0)
cal_error(test)
test <- calABM2(sD,1,use_parallel=F,nu=100,p=0.0,beta=1,r=0.05,eta=0.,tau=0.0)
cal_error(test)
#only behavioural factors in heating system failures - no upgrades
test <- calABM2(sD,1,use_parallel=F,nu=0,p=0.,beta=1,r=0.05,eta=0.,tau=0.0)
cal_error(test)

#scans - nu dependence of heat pump uptake from failures
scan <- tibble()
for(p in seq(0.0,0.04,by=0.02))
for(nu in seq(0,1,by=0.1)) {
  print(paste("nu=",nu,"p=",p))
  res <- calABM2(sD,1,use_parallel=F,nu=nu,p=p,beta=1,r=0.05,eta=0.,tau=0.0)
  calerror <- cal_error(res)
  sc <- res$parameters
  sc$error <- calerror
  scan <- scan %>% bind_rows(sc)
}
#write_csv(scan,"~/Policy/CAMG/EED/Heat/data/new_calibrations/scans/nu_p_scan.csv")
scan <- read_csv("~/Policy/CAMG/EED/Heat/data/new_calibrations/scans/nu_p_scan.csv")
scan %>% ggplot(aes(nu.,n_heat_pump_grant,colour=factor(p.)))+geom_line()+geom_hline(yintercept=18000)
scan %>% ggplot(aes(nu.,error,colour=factor(p.)))+geom_line()

#
scan <- tibble()
for(p in seq(0.0,0.04,by=0.02))
  for(nu in seq(0,1,by=0.1)) {
    print(paste("nu=",nu,"p=",p))
    res <- calABM2(sD,1,use_parallel=F,nu=nu,p=p,beta=1,r=0.05,eta=0.05,tau=0.0)
    scan <- scan %>% bind_rows(res$parameters)
    scan$error <- cal_error(res)
  }
#write_csv(scan,"~/Policy/CAMG/EED/Heat/data/new_calibrations/scans/nu_p_scan.csv")
scan %>% ggplot(aes(nu.,n_heat_pump_grant,colour=factor(p.)))+geom_line()+geom_hline(yintercept=18000)

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


