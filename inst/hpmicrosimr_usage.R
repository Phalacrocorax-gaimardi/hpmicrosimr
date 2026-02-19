#hpmicrosimr usage
library(hpmicrosimr)
sessionInfo()
library(tidyverse)
library(ggthemes)
library(xtable)

sD_wem <- readxl::read_xlsx("C:/Users/Joe/pkgs/hpmicrosimr/inst/extdata/scenario_parameters.xlsx",sheet="WEM")
sD_wam <- readxl::read_xlsx("C:/Users/Joe/pkgs/hpmicrosimr/inst/extdata/scenario_parameters.xlsx",sheet="WAM")
sD_cap <- readxl::read_xlsx("C:/Users/Joe/pkgs/hpmicrosimr/inst/extdata/scenario_parameters.xlsx",sheet="CAP")
sD_dwit <- readxl::read_xlsx("C:/Users/Joe/pkgs/hpmicrosimr/inst/extdata/scenario_parameters.xlsx",sheet="DWIT")



sD_wam[sD_wam$parameter=="grant_increase_date","value"]
sD_wam[sD_wem$parameter=="grant_increase_factor","value"]
#sD_wem[sD_wem$parameter=="better_energy_introduction","value"] <- 2050
#sD_wem[sD_wem$parameter=="oss_introduction","value"] <- 2050

test <- runABM(sD_wam,4,2040)
#wam <- test
#saveRDS(wam,"~/Policy/CAMG/EED/Heat/data/wam.RData")

params <- scenario_params(sD_wem,2026)
params$warmer_homes_introduction

########
sD_cal <- readxl::read_xlsx("C:/Users/Joe/pkgs/hpmicrosimr/inst/extdata/scenario_parameters.xlsx",sheet="WEM")

sD_cal[sD_cal$parameter=="nu.","value"] <- calib$nu
sD_cal[sD_cal$parameter=="p.","value"] <- calib$p
sD_cal[sD_cal$parameter=="r.","value"] <- 0.03
sD_cal[sD_cal$parameter=="beta.","value"] <- calib$beta
sD_cal[sD_cal$parameter=="eta.","value"] <- calib$eta
sD_cal[sD_cal$parameter=="tau.","value"] <- calib$tau
sD_cal[sD_cal$parameter=="lambda.","value"] <- 0


#test <- runABM(sD_cal,4,2040)

wam <- readRDS("~/Policy/CAMG/EED/Heat/data/wam.RData")
wem <- readRDS("~/Policy/CAMG/EED/Heat/data/wem.RData")
cap <- readRDS("~/Policy/CAMG/EED/Heat/data/cap.RData")
dwit <- readRDS("~/Policy/CAMG/EED/Heat/data/dwit.RData")

abm <- wam[[1]]
n_run <- wem[[3]] %>% filter(parameter=="Nrun") %>% pull(value)
df0 <- abm %>% group_by(simulation,date) %>% summarise(n0=n())
housing_stock_oo <- 1147552 #census

test2 <- abm %>% group_by(simulation,date,tech) %>% summarise(n=n()) %>% inner_join(df0)
test2 <- test2 %>% mutate(n=n/n0*housing_stock_oo) %>% select(-n0)
test2 <- test2 %>% group_by(tech,date) %>% summarise(n=mean(n))
test2 <- test2 %>% ungroup() %>% arrange(date,factor(tech,levels = c("heat_pump","electricity","oil","gas","solid_fuel")))
test2 <- test2 %>% mutate(tech=factor(tech,levels=rev(c("heat_pump","electricity","oil","gas","solid_fuel"))))

g <- test2 %>% ggplot(aes(date,n,fill=tech))+geom_area()
g <- g + theme_minimal() + scale_fill_viridis_d()
g
#export::graph2ppt(g,"~/Policy/CAMG/EED/Heat/wam_uptake.ppt")
#annual year end number fo heat pumps
ntech <- test2 %>% filter(tech=="heat_pump",str_detect(date,"-01-01")) %>% mutate(year_end=year(date)) %>% select(-date)
#write_csv(ntech,"~/Policy/CAMG/EED/Heat/data/ntech_cap.csv")

ntech_wem <- read_csv("~/Policy/CAMG/EED/Heat/data/ntech_wem.csv") %>% rename("wem"=n)
ntech_wam <- read_csv("~/Policy/CAMG/EED/Heat/data/ntech_wam.csv") %>% rename("wam"=n)
ntech_cap <- read_csv("~/Policy/CAMG/EED/Heat/data/ntech_cap.csv") %>% rename("cap"=n)
ntech_dwit <- read_csv("~/Policy/CAMG/EED/Heat/data/ntech_dwit.csv") %>% rename("dwit"=n)
#
ntech <- ntech_wem %>% inner_join(ntech_wam) %>% inner_join(ntech_cap) %>% inner_join(ntech_dwit) %>% filter(tech=="heat_pump") %>% select(-tech)
ntech <- ntech %>% mutate(wem=round(wem/1000),wam=round(wam/1000),cap=round(cap/1000),dwit=round(dwit/1000))
ntech <- ntech[,c(2,1,3,4,5)]
test <- ntech %>% filter(year_end %in% c(2019,2025,2030,2035,2040))

xt <- xtable(
  test,
  caption = "Projected number of heat pumps installed in pre-2015 private housing stock",
  label = "tab:heat_pumps",
  digits=0,
  align = c("l", "l","r", "r","r","r")
)

print(
  xt,
  include.rownames = FALSE,
  booktabs = TRUE,
  sanitize.text.function = identity,
  caption.placement = "top"
)


#number of b2s = wem vs wam
abm <- wem[[1]]
nb2_wem <- abm %>% filter(ber <= 125) %>% group_by(simulation,date) %>% summarise(n_b2 = n()) %>% inner_join(df0)
nb2_wem <- nb2_wem %>% mutate(n_b2=n_b2/n0*housing_stock_oo) %>% select(-n0)
nb2_wem <- nb2_wem %>% group_by(date) %>% summarise(wem=mean(n_b2))

abm <- wam[[1]]
nb2_wam <- abm %>% filter(ber <= 125) %>% group_by(simulation,date) %>% summarise(n_b2 = n()) %>% inner_join(df0)
nb2_wam <- nb2_wam %>% mutate(n_b2=n_b2/n0*housing_stock_oo) %>% select(-n0)
nb2_wam <- nb2_wam %>% group_by(date) %>% summarise(wam=mean(n_b2))

abm <- cap[[1]]
nb2_cap <- abm %>% filter(ber <= 125) %>% group_by(simulation,date) %>% summarise(n_b2 = n()) %>% inner_join(df0)
nb2_cap <- nb2_cap %>% mutate(n_b2=n_b2/n0*housing_stock_oo) %>% select(-n0)
nb2_cap <- nb2_cap %>% group_by(date) %>% summarise(cap=mean(n_b2))

abm <- dwit[[1]]
nb2_dwit <- abm %>% filter(ber <= 125) %>% group_by(simulation,date) %>% summarise(n_b2 = n()) %>% inner_join(df0)
nb2_dwit <- nb2_dwit %>% mutate(n_b2=n_b2/n0*housing_stock_oo) %>% select(-n0)
nb2_dwit <- nb2_dwit %>% group_by(date) %>% summarise(dwit=mean(n_b2))


nb2 <- nb2_wem %>% inner_join(nb2_wam) %>% inner_join(nb2_cap) %>% inner_join(nb2_dwit)


nb2 <- nb2 %>% pivot_longer(cols=-date="cenaio")

g <- nb2 %>% ggplot(aes(date,value/1000,colour=name))+geom_line(linewidth=1.25) + theme_minimal() #+ theme(legend.position = "none")
g <- g + scale_colour_canva()
#export::graph2ppt(g,"~/Policy/CAMG/EED/Heat/nb2s.ppt")

#table

nb2 <- nb2 %>% rename("scenario"=name) %>% mutate(value = value/1000)
nb2 <- nb2 %>% filter(str_detect(date,"-01-01")) %>% mutate(year=year(date)) %>% select(-date)
nb2[,-5] <- nb2[,-5]/1000
#nb2 <- nb2 %>% pivot_wider(names_from=scenario,values_from=value)
test <- nb2 %>% filter(year %in% c(2015,2020,2025,2030,2040)) %>% select(year,wem,wam,cap,dwit)
xt <- xtable(
  test,
  caption = "Projected number of B2 (in thousands) or better ratings in pre-2015 housing stock",
  label = "tab:nb2s",
  digits=0,
  align = c("l","l", "r","r", "r","r")
)

print(
  xt,
  include.rownames = FALSE,
  booktabs = TRUE,
  sanitize.text.function = identity,
  caption.placement = "top"
)


#mean HLI vs mean BER
abm <- wem[[1]]
rating <- abm %>% group_by(simulation,date) %>% summarise(ber=mean(ber),hli=mean(hli)) %>% group_by(date) %>% summarise(hli=mean(hli),ber=mean(ber))
rating$scenario <- "WEM"
#
rating_wem <- rating

abm <- wam[[1]]
rating <- abm %>% group_by(simulation,date) %>% summarise(ber=mean(ber),hli=mean(hli)) %>% group_by(date) %>% summarise(hli=mean(hli),ber=mean(ber))
rating$scenario <- "WAM"
#
rating_wam <- rating

abm <- cap[[1]]
rating <- abm %>% group_by(simulation,date) %>% summarise(ber=mean(ber),hli=mean(hli)) %>% group_by(date) %>% summarise(hli=mean(hli),ber=mean(ber))
rating$scenario <- "CAP"
#
rating_cap <- rating

abm <- dwit[[1]]
rating <- abm %>% group_by(simulation,date) %>% summarise(ber=mean(ber),hli=mean(hli)) %>% group_by(date) %>% summarise(hli=mean(hli),ber=mean(ber))
rating$scenario <- "DWIT"
#
rating_dwit <- rating


rating <- bind_rows(rating_wem,rating_wam,rating_cap,rating_dwit)
rating$scenario <- factor(rating$scenario, levels = c("WEM","WAM","CAP","DWIT"))
g <- rating %>% ggplot(aes(date,hli,colour=scenario))+geom_line(linewidth=1.25) + theme_minimal() #+ theme(legend.position = "none")
g1 <- g + scale_colour_canva(palette="Fun and cheerful") + scale_y_continuous(limits=c(0,3)) + theme(legend.position = "none")
g <- rating %>% ggplot(aes(date,ber,colour=scenario))+geom_line(linewidth=1.25) + theme_minimal() #+ theme(legend.position = "none")
g2 <- g + scale_colour_canva(palette="Fun and cheerful") + scale_y_continuous(limits=c(0,300))
g1+g2
#export::graph2ppt(g1+g2,"~/Policy/CAMG/EED/Heat/mean_hli_ber.ppt")


test2 <- abm %>% group_by(date,tech) %>% summarise(ber=mean(ber),hli=mean(hli))
test2 <- test2 %>% ungroup() %>% arrange(date,factor(tech,levels = c("heat_pump","electricity","oil","gas","solid_fuel")))
test2 <- test2 %>% mutate(tech=factor(tech,levels=rev(c("heat_pump","electricity","oil","gas","solid_fuel"))))
housing_stock_oo <- 611877+535675
n_0 <- length(abm$serial %>% unique())
g <- test2 %>% ggplot(aes(date,ber,colour=tech))+geom_line(linewidth=1.2)
g <- g + theme_minimal() + scale_colour_viridis_d()

g1 <- test2 %>% ggplot(aes(date,hli,colour=tech))+geom_line(linewidth=1.2)
g1 <- g1 + theme_minimal() + scale_colour_viridis_d()
library(patchwork)
g+g1
#export::graph2ppt(g+g1,"~/Policy/CAMG/EED/Heat/test_ber_hli.ppt")

#number of heat pump adoptions as part of home energy upgrade vs system failures
#annual heat pump
test2 <- test[[1]] %>% filter(heat_pump_grant > 0) %>% group_by(year = year(date), failure) %>% summarise(n=n())
test2 <- test2 %>% group_by(failure) %>% mutate(n=cumsum(n))
#
test2 %>% ggplot(aes(year,n,colour=failure))+geom_line()

test2 <- test[[1]] %>% filter(heat_pump_grant > 0) %>% group_by(year = year(date), grant_type, failure) %>% summarise(n=n())
test2 <- test2 %>% group_by(grant_type, failure) %>% mutate(n=cumsum(n))
test2 %>% ggplot(aes(year,n,colour=grant_type, linetype=failure))+geom_line()

#################
# annual means or end of year
##############
#tech installed
df0 <- abm %>% group_by(simulation,date) %>% summarise(n0=n())
test2 <- abm %>% group_by(simulation,date,tech) %>% summarise(n=n()) %>% inner_join(df0)
test2 <- test2 %>% mutate(n=n/n0*housing_stock_oo) %>% select(-n0)
test2 <- test2 %>% group_by(tech,date) %>% summarise(n=mean(n))
test2 %>% filter(date %in% c("2015-01-01","2026-01-01","2031-01-01","2036-01-01","2040-01-01"))
#grant type
df0 <- abm %>% group_by(simulation,date) %>% summarise(n0=n())
test2 <- abm %>% group_by(simulation,date,grant_type) %>% summarise(n=n()) %>% inner_join(df0)
test2 <- test2 %>% mutate(n=n/n0*housing_stock_oo) %>% select(-n0)
test2 <- test2 %>% group_by(grant_type,date) %>% summarise(n=mean(n))
test2 %>% filter(!is.na(grant_type)) %>% ggplot(aes(date,n,colour=grant_type))+geom_line()

#
g1 <- abm %>% group_by(simulation,date) %>% summarise(eac_actual=mean(eac)) %>% ggplot(aes(date,eac_actual,colour=factor(simulation)))+geom_line() + theme(legend.position="none")
g2 <- abm %>% group_by(simulation,date) %>% summarise(eac_actual=mean(eac_actual)) %>% ggplot(aes(date,eac_actual,colour=factor(simulation)))+geom_line() + theme(legend.position="none")
g1+g2

######################
# FEC
##########################
# FEC is different from space heating requirement
# adjust for heat pump energy use
########################################

params0 <- scenario_params(sD,2025)

abm <- wem[[1]]
abm <- abm %>% mutate(space_heating_requirement_actual = space_heating_requirement(hli,floor_area,rebound=0.4,params0))
abm <- abm %>% mutate(space_heating_requirement_theory = space_heating_requirement(hli,floor_area,rebound=0,params0))
abm <- abm %>% mutate(temperature_deficit = (space_heating_requirement_theory-space_heating_requirement_actual)/(8.76*hli*floor_area))
abm <- abm %>% mutate(income_tercile = case_when(income < 37500~"low",income > 62400~"high",(income <= 62400) & (income >= 37500)~"middle"))

#heat <- abm %>% group_by(simulation,date) %>% summarise(space_heating_requirement = sum(space_heating_requirement_theory))
#heat %>% ggplot(aes(date,space_heating_requirement,colour=factor(simulation))) + geom_line() + theme(legend.position="none")
#
temperature <- abm %>% group_by(simulation,date,income_tercile) %>% summarise(degree_deficit = mean(temperature_deficit))
temperature <- temperature %>% group_by(income_tercile,date) %>% summarise(degree_deficit=mean(degree_deficit))
temperature$scenario <- "WEM"
t_wem <- temperature


abm <- wam[[1]]
abm <- abm %>% mutate(space_heating_requirement_actual = space_heating_requirement(hli,floor_area,rebound=0.4,params0))
abm <- abm %>% mutate(space_heating_requirement_theory = space_heating_requirement(hli,floor_area,rebound=0,params0))
abm <- abm %>% mutate(temperature_deficit = (space_heating_requirement_theory-space_heating_requirement_actual)/(8.76*hli*floor_area))
abm <- abm %>% mutate(income_tercile = case_when(income < 37500~"low",income > 62400~"high",(income <= 62400) & (income >= 37500)~"middle"))

#heat <- abm %>% group_by(simulation,date) %>% summarise(space_heating_requirement = sum(space_heating_requirement_theory))
#heat %>% ggplot(aes(date,space_heating_requirement,colour=factor(simulation))) + geom_line() + theme(legend.position="none")
#
temperature <- abm %>% group_by(simulation,date,income_tercile) %>% summarise(degree_deficit = mean(temperature_deficit))
temperature <- temperature %>% group_by(income_tercile,date) %>% summarise(degree_deficit=mean(degree_deficit))
temperature$scenario <- "WAM"
t_wam <- temperature


abm <- cap[[1]]
abm <- abm %>% mutate(space_heating_requirement_actual = space_heating_requirement(hli,floor_area,rebound=0.4,params0))
abm <- abm %>% mutate(space_heating_requirement_theory = space_heating_requirement(hli,floor_area,rebound=0,params0))
abm <- abm %>% mutate(temperature_deficit = (space_heating_requirement_theory-space_heating_requirement_actual)/(8.76*hli*floor_area))
abm <- abm %>% mutate(income_tercile = case_when(income < 37500~"low",income > 62400~"high",(income <= 62400) & (income >= 37500)~"middle"))

#heat <- abm %>% group_by(simulation,date) %>% summarise(space_heating_requirement = sum(space_heating_requirement_theory))
#heat %>% ggplot(aes(date,space_heating_requirement,colour=factor(simulation))) + geom_line() + theme(legend.position="none")
#
temperature <- abm %>% group_by(simulation,date,income_tercile) %>% summarise(degree_deficit = mean(temperature_deficit))
temperature <- temperature %>% group_by(income_tercile,date) %>% summarise(degree_deficit=mean(degree_deficit))
temperature$scenario <- "CAP"
t_cap <- temperature



abm <- dwit[[1]]
abm <- abm %>% mutate(space_heating_requirement_actual = space_heating_requirement(hli,floor_area,rebound=0.4,params0))
abm <- abm %>% mutate(space_heating_requirement_theory = space_heating_requirement(hli,floor_area,rebound=0,params0))
abm <- abm %>% mutate(temperature_deficit = (space_heating_requirement_theory-space_heating_requirement_actual)/(8.76*hli*floor_area))
abm <- abm %>% mutate(income_tercile = case_when(income < 37500~"low",income > 62400~"high",(income <= 62400) & (income >= 37500)~"middle"))

#heat <- abm %>% group_by(simulation,date) %>% summarise(space_heating_requirement = sum(space_heating_requirement_theory))
#heat %>% ggplot(aes(date,space_heating_requirement,colour=factor(simulation))) + geom_line() + theme(legend.position="none")
#
temperature <- abm %>% group_by(simulation,date,income_tercile) %>% summarise(degree_deficit = mean(temperature_deficit))
temperature <- temperature %>% group_by(income_tercile,date) %>% summarise(degree_deficit=mean(degree_deficit))
temperature$scenario <- "DWIT"
t_dwit <- temperature

t_all <- t_wem %>% bind_rows(t_wam) %>% bind_rows(t_cap) %>% bind_rows(t_dwit)
t_all$scenario <- factor(t_all$scenario,levels=c("WEM","WAM","CAP","DWIT"))
t_all$income_tercile <- factor(t_all$income_tercile,levels=c("low","middle","high"))

g <- t_all %>% ggplot(aes(date,-degree_deficit,colour=income_tercile)) + geom_line(linewidth=2)
g <- g + theme_minimal() + scale_colour_canva(palette="Fun and cheerful") + facet_wrap(.~scenario, nrow=1)
#export::graph2ppt(g,"~/Policy/CAMG/EED/Heat/degree_deficits.ppt")



temperature <- abm %>% filter(income < 37500) %>% group_by(simulation,date,income_tercile) %>% summarise(degree_deficit = mean(temperature_deficit))
g1 <- temperature %>% ggplot(aes(date,-degree_deficit,colour=factor(simulation))) + geom_line()
g1 <- g1 + scale_y_continuous(limits=c(-1.2,-0.65)) + theme_minimal() + theme(legend.position="none")

temperature <- abm %>% filter(income > 62400) %>% group_by(simulation,date) %>% summarise(degree_deficit = mean(temperature_deficit))
g2 <- temperature %>% ggplot(aes(date,-degree_deficit,colour=factor(simulation))) + geom_line()
g2 <- g2 + scale_y_continuous(limits=c(-1.2,-0.65))  + theme_minimal() + theme(legend.position="none")
#
temperature <- abm %>% filter(income <= 62400 & income >= 37500) %>% group_by(simulation,date) %>% summarise(degree_deficit = mean(temperature_deficit))
g3 <- temperature %>% ggplot(aes(date,-degree_deficit,colour=factor(simulation))) + geom_line()
g3 <- g3 + scale_y_continuous(limits=c(-1.2,-0.65)) + theme_minimal() + theme(legend.position="none")

#
library(patchwork)
g1+g3+g2
#export::graph2ppt(g1+g3+g2,"~/Policy/CAMG/EED/Heat/degree_deficit_wem.ppt")

###############################
# space-heating FEC households
###############################

#abm <- wem[[1]]
params0 <- scenario_params(sD,2025)

abm <- wem[[1]]
abm <- abm %>% mutate(space_heating_requirement_actual = space_heating_requirement(hli,floor_area,rebound=0.4,params0))
abm <- abm %>% mutate(space_heating_requirement_theory = space_heating_requirement(hli,floor_area,rebound=0,params0))
abm <- abm %>% mutate(temperature_deficit = (space_heating_requirement_theory-space_heating_requirement_actual)/(8.76*hli*floor_area))


abm <- abm %>% mutate(efficiency = heating_system_efficiency(tech,heating_install_time))
df <- abm %>% group_by(tech,date) %>% summarise(efficiency=mean(efficiency))
#df %>% ggplot(aes(date,efficiency,colour=tech))+geom_line()
#FEC
abm <- abm %>% mutate(fec_prebound=space_heating_requirement_actual/efficiency, fec_theory=space_heating_requirement_theory/efficiency)
#totals
fec <- abm %>% group_by(simulation,date) %>% summarise(fec_prebound=sum(fec_prebound),fec_theory=sum(fec_theory)) %>% inner_join(df0)
fec <- fec %>% mutate(fec_prebound=fec_prebound/n0*housing_stock_oo, fec_theory=fec_theory/n0*housing_stock_oo)
fec <- fec %>% select(-n0) %>% group_by(simulation,year=year(date)) %>% summarise(fec_prebound=mean(fec_prebound),fec_theory=mean(fec_theory))
#g1 <- fec %>% ggplot(aes(year,fec_theory/10^9,colour=factor(simulation)))+geom_line() + theme_minimal() + theme(legend.position="none")
#g1 <- g1 + scale_y_continuous(limits=c(15,32))
fec <- fec %>% mutate(fec_prebound=fec_prebound/10^9)
fec_wem <- fec
fec_wem$scenario <- "WEM"


abm <- wam[[1]]
abm <- abm %>% mutate(space_heating_requirement_actual = space_heating_requirement(hli,floor_area,rebound=0.4,params0))
abm <- abm %>% mutate(space_heating_requirement_theory = space_heating_requirement(hli,floor_area,rebound=0,params0))
abm <- abm %>% mutate(temperature_deficit = (space_heating_requirement_theory-space_heating_requirement_actual)/(8.76*hli*floor_area))


abm <- abm %>% mutate(efficiency = heating_system_efficiency(tech,heating_install_time))
df <- abm %>% group_by(tech,date) %>% summarise(efficiency=mean(efficiency))
#df %>% ggplot(aes(date,efficiency,colour=tech))+geom_line()
#FEC
abm <- abm %>% mutate(fec_prebound=space_heating_requirement_actual/efficiency, fec_theory=space_heating_requirement_theory/efficiency)
#totals
fec <- abm %>% group_by(simulation,date) %>% summarise(fec_prebound=sum(fec_prebound),fec_theory=sum(fec_theory)) %>% inner_join(df0)
fec <- fec %>% mutate(fec_prebound=fec_prebound/n0*housing_stock_oo, fec_theory=fec_theory/n0*housing_stock_oo)
fec <- fec %>% select(-n0) %>% group_by(simulation,year=year(date)) %>% summarise(fec_prebound=mean(fec_prebound),fec_theory=mean(fec_theory))
#g1 <- fec %>% ggplot(aes(year,fec_theory/10^9,colour=factor(simulation)))+geom_line() + theme_minimal() + theme(legend.position="none")
#g1 <- g1 + scale_y_continuous(limits=c(15,32))
fec <- fec %>% mutate(fec_prebound=fec_prebound/10^9)
fec_wam <- fec
fec_wam$scenario <- "WAM"

abm <- cap[[1]]
abm <- abm %>% mutate(space_heating_requirement_actual = space_heating_requirement(hli,floor_area,rebound=0.4,params0))
abm <- abm %>% mutate(space_heating_requirement_theory = space_heating_requirement(hli,floor_area,rebound=0,params0))
abm <- abm %>% mutate(temperature_deficit = (space_heating_requirement_theory-space_heating_requirement_actual)/(8.76*hli*floor_area))


abm <- abm %>% mutate(efficiency = heating_system_efficiency(tech,heating_install_time))
df <- abm %>% group_by(tech,date) %>% summarise(efficiency=mean(efficiency))
#df %>% ggplot(aes(date,efficiency,colour=tech))+geom_line()
#FEC
abm <- abm %>% mutate(fec_prebound=space_heating_requirement_actual/efficiency, fec_theory=space_heating_requirement_theory/efficiency)
#totals
fec <- abm %>% group_by(simulation,date) %>% summarise(fec_prebound=sum(fec_prebound),fec_theory=sum(fec_theory)) %>% inner_join(df0)
fec <- fec %>% mutate(fec_prebound=fec_prebound/n0*housing_stock_oo, fec_theory=fec_theory/n0*housing_stock_oo)
fec <- fec %>% select(-n0) %>% group_by(simulation,year=year(date)) %>% summarise(fec_prebound=mean(fec_prebound),fec_theory=mean(fec_theory))
#g1 <- fec %>% ggplot(aes(year,fec_theory/10^9,colour=factor(simulation)))+geom_line() + theme_minimal() + theme(legend.position="none")
#g1 <- g1 + scale_y_continuous(limits=c(15,32))
fec <- fec %>% mutate(fec_prebound=fec_prebound/10^9)
fec_cap <- fec
fec_cap$scenario <- "CAP"


abm <- dwit[[1]]
abm <- abm %>% mutate(space_heating_requirement_actual = space_heating_requirement(hli,floor_area,rebound=0.4,params0))
abm <- abm %>% mutate(space_heating_requirement_theory = space_heating_requirement(hli,floor_area,rebound=0,params0))
abm <- abm %>% mutate(temperature_deficit = (space_heating_requirement_theory-space_heating_requirement_actual)/(8.76*hli*floor_area))


abm <- abm %>% mutate(efficiency = heating_system_efficiency(tech,heating_install_time))
df <- abm %>% group_by(tech,date) %>% summarise(efficiency=mean(efficiency))
#df %>% ggplot(aes(date,efficiency,colour=tech))+geom_line()
#FEC
abm <- abm %>% mutate(fec_prebound=space_heating_requirement_actual/efficiency, fec_theory=space_heating_requirement_theory/efficiency)
#totals
fec <- abm %>% group_by(simulation,date) %>% summarise(fec_prebound=sum(fec_prebound),fec_theory=sum(fec_theory)) %>% inner_join(df0)
fec <- fec %>% mutate(fec_prebound=fec_prebound/n0*housing_stock_oo, fec_theory=fec_theory/n0*housing_stock_oo)
fec <- fec %>% select(-n0) %>% group_by(simulation,year=year(date)) %>% summarise(fec_prebound=mean(fec_prebound),fec_theory=mean(fec_theory))
#g1 <- fec %>% ggplot(aes(year,fec_theory/10^9,colour=factor(simulation)))+geom_line() + theme_minimal() + theme(legend.position="none")
#g1 <- g1 + scale_y_continuous(limits=c(15,32))
fec <- fec %>% mutate(fec_prebound=fec_prebound/10^9)
fec_dwit <- fec
fec_dwit$scenario <- "DWIT"

fec <- bind_rows(fec_wem,fec_wam,fec_cap,fec_dwit)
fec$scenario <- factor(fec$scenario,levels=c("WEM","WAM","CAP","DWIT"))
#

fec_mean <- fec %>% group_by(scenario,year) %>% summarise(fec_prebound=mean(fec_prebound))

g2 <- fec_mean %>% ggplot(aes(year,fec_prebound,colour=scenario))+geom_line(linewidth=1.25) + theme_minimal() #+ theme(legend.position="none")
g2 <- g2 + scale_y_continuous(limits=c(0,25)) + scale_colour_canva(palette="Fun and cheerful")
g2
#export::graph2ppt(g2,"~/Policy/CAMG/EED/Heat/fec_scens.ppt")

#EED
fec %>% filter(year==2022) %>% pull(fec_prebound) %>% mean()/1e+9 #20TWh
fec %>% filter(year==2030) %>% pull(fec_prebound) %>% mean()/1e+9 #17.9 TWh
#1-17.9/20 -10.5 vs -12.6% target - inadequate

fec %>% group_by(year) %>% summarise(fec_prebound=mean(fec_prebound)/1e+9,fec_theory=mean(fec_theory)/1e+9)


#########################
#make a table of cumulative fec emissions
##############################

fec_mean  <- fec %>% group_by(scenario,year) %>% summarise(fec=mean(fec_prebound)) %>% filter(year > 2020)
fec_cumul <- fec_mean %>% ungroup() %>% group_by(scenario) %>% mutate(fec=cumsum(fec))

test <- fec_cumul %>% pivot_wider(names_from=scenario,values_from=fec) %>% filter(year %in% c(2025,2030,2035,2040))
xt <- xtable(
  test,
  caption = "Cumulative CO2 Emissions",
  label = "tab:co2_cumulative",
  digits=1,
  align = c("l", "l","r", "r","r","r")
)

print(
  xt,
  include.rownames = FALSE,
  booktabs = TRUE,
  sanitize.text.function = identity,
  caption.placement = "top"
)

##################################
# co2 emissions
################################

emissions_factors <- read_csv("C:/Users/Joe/pkgs/hpmicrosimr/inst/extdata/tech_emissions_factors.csv")
abm <- dwit[[1]]

abm <- abm %>% mutate(space_heating_requirement_actual = space_heating_requirement(hli,floor_area,rebound=0.4,params0))
abm <- abm %>% mutate(space_heating_requirement_theory = space_heating_requirement(hli,floor_area,rebound=0,params0))
abm <- abm %>% mutate(temperature_deficit = (space_heating_requirement_theory-space_heating_requirement_actual)/(8.76*hli*floor_area))

abm <- abm %>% inner_join(emissions_factors)
abm <- abm %>% mutate(tco2=gCO2_per_kWh*space_heating_requirement_actual/1e+6)
#
df <- abm %>% group_by(simulation,date) %>% summarise(tco2=sum(tco2)) %>% inner_join(df0) %>% mutate(Mtco2=tco2/n0*housing_stock_oo/1e+6)
df <- df %>% select(-n0,-tco2)

df$scenario <- "DWIT"
df_dwit <- df
co2_all <- bind_rows(df_wem,df_wam,df_cap,df_dwit)
#
co2_all$scenario <- factor(co2_all$scenario,levels = c("WEM","WAM","CAP","DWIT"))

co2_all <- co2_all %>% filter(str_detect(date,"11-01")) %>% mutate(year=year(date)) %>% select(-date)
co2_new <- completions %>% select(year,Mtco2_wem) %>% rename("Mtco2_new"=Mtco2_wem)

#fan FEC plots
co2_q <- co2_all %>%
  group_by(scenario,year) %>%
  summarise(
    q10 = quantile(Mtco2, 0.10),
    q20 = quantile(Mtco2, 0.20),
    q30 = quantile(Mtco2, 0.30),
    q40 = quantile(Mtco2, 0.40),
    q50 = quantile(Mtco2, 0.50),
    q60 = quantile(Mtco2, 0.60),
    q70 = quantile(Mtco2, 0.70),
    q80 = quantile(Mtco2, 0.80),
    q90 = quantile(Mtco2, 0.90)
  )

g2 <- ggplot( co2_q %>% filter(scenario %in% c("CAP","DWIT")), aes(year)) +
  geom_ribbon(aes(ymin = q10, ymax = q90),
              fill = "grey90") +
  geom_ribbon(aes(ymin = q20, ymax = q80),
              fill = "grey75") +
  geom_ribbon(aes(ymin = q30, ymax = q70),
              fill = "grey60") +
  geom_ribbon(aes(ymin = q40, ymax = q60),
              fill = "grey45") +
  geom_line(aes(y = q50), linewidth = 1) +
  theme_minimal() + facet_wrap(.~scenario)

#export::graph2ppt(g1+g2,"~/Policy/CAMG/EED/Heat/emissions_wem_wam.ppt")
#TIMES model RSD_Services_CO2Emissions covers commercial and residential hotwater, cooking emissions
g2 +geom_line(data=tim_300mt_led %>% filter(year <= 2040),aes(year,0.63*Mtco2),colour="red")


co2_all_mean <- co2_all %>% group_by(scenario,year) %>% summarise(Mtco2_old=mean(Mtco2)) %>% ungroup() %>% inner_join(co2_new,by="year")
#
co2_all_mean <- co2_all_mean %>% left_join(tim_300mt_led %filter(year >= 2021))
#add epa
epa <- read_csv("~/Policy/CAMG/EED/Heat/data/epa_residential_co2.csv") %>% mutate(epa=ktCO2/1000)
co2_all_mean <- co2_all_mean %>% left_join(epa)

g <- co2_all_mean %>% ggplot()+geom_line(aes(year,Mtco2_old+Mtco2_new,colour=scenario),linewidth=1.25)+scale_colour_canva(palette="Fun and cheerful") + theme_minimal()
g <- g + geom_point(data=co2_all_mean,aes(year,0.7*epa),colour="grey80")
esr <- tibble(x=2021,xend=2030,y=4.383,yend=4.383*(1-0.37)) #2030 target 18.2 TWh
g <- g + geom_segment(data=esr,aes(x=x,y=y,xend=xend,yend=yend), linewidth=1,colour="grey50",linetype="dashed") + geom_point(data=esr,aes(xend,yend),size=2)
#g <- g + geom_hline(yintercept = 4.383*(1-0.37), linetype="dotted") #+ scale_y_continuous(trans="sqrt")
g <- g + coord_cartesian(ylim=c(0,5)) #+ theme(axis.text.x = element_text(angle=-90))
#export::graph2ppt(g,"~/Policy/CAMG/EED/Heat/co2_target.ppt")

#emissions table & esr
co2_all_mean %>% filter(year %in% c(2021,2025,2030,2040)) %>% arrange(year)

co2_cumul <- co2_all_mean %>% filter(year > 2020) %>% group_by(scenario) %>% mutate(Mtco2=cumsum(Mtco2_old))
test <- co2_cumul %>% filter(year %in% c(2025,2030,2035,2040)) %>% select(scenario,year,Mtco2)  %>% arrange(year) %>% select(year,scenario,Mtco2)
test <- test %>% pivot_wider(names_from=scenario,values_from=Mtco2)

xt <- xtable(
  test,
  caption = "Cumulative CO2 Emissions",
  label = "tab:co2_cumulative",
  digits=1,
  align = c("l", "l","r", "r","r","r")
)

print(
  xt,
  include.rownames = FALSE,
  booktabs = TRUE,
  sanitize.text.function = identity,
  caption.placement = "top"
)

esr_co2 <- co2_all_mean %>% filter(year %in% 2021:2030)
#
esr_co2 <- esr_co2 %>% mutate(aea)

#emissions table (carbon budgets)
#co2 <- df %>% group_by(date) %>% summarise(Mtco2=mean(Mtco2)) %>% filter(str_detect(date,"-11-01")) %>% mutate(year=year(date))


co2_mean_all %>% filter(year %in% c(2021,2025,2030,2035,2040)) %>% select(-date)

#ESR targets - 2021 reference 43.48Mt
#2030 taregt 27.7
#-37% reduction vs 2021


#include emissions from new builds (see below)

#emissions by income
df <- abm %>% filter(income < 37500 ) %>% group_by(simulation,date) %>% summarise(tco2=sum(tco2)) %>% inner_join(df0) %>% mutate(Mtco2=tco2/n0*housing_stock_oo/1e+6)


g <- df %>% ggplot(aes(date,Mtco2,colour=factor(simulation)))+geom_line()
g <- g + theme_minimal() + theme(legend.position="none") + scale_y_continuous(limits=c(0.5,2))
g1 <- g
df <- abm %>% filter(income >= 37500,income <= 62400 ) %>% group_by(simulation,date) %>% summarise(tco2=sum(tco2)) %>% inner_join(df0) %>% mutate(Mtco2=tco2/n0*housing_stock_oo/1e+6)
g <- df %>% ggplot(aes(date,Mtco2,colour=factor(simulation)))+geom_line()
g <- g + theme_minimal() + theme(legend.position="none") + scale_y_continuous(limits=c(0.5,2))
g2 <- g
df <- abm %>% filter(income >= 62400) %>% group_by(simulation,date) %>% summarise(tco2=sum(tco2)) %>% inner_join(df0) %>% mutate(Mtco2=tco2/n0*housing_stock_oo/1e+6)
g <- df %>% ggplot(aes(date,Mtco2,colour=factor(simulation)))+geom_line()
g <- g + theme_minimal() + theme(legend.position="none") + scale_y_continuous(limits=c(0.5,2))
g3 <- g

g1+g2+g3

#fec by income


###############################
# grants, upgrades, failures
###############################

abm %>% filter(failure) %>% group_by(simulation,date) %>% summarise(n=n()) %>% mutate(n=cumsum(n)) #%>% ggplot(aes(date,n,group=interaction(simulation),colour=)+geom_line()
#cumulative grant scheme uptake upgrades
alpha_0 <- 0.2
abm  <- wem[[1]]
g1 <- abm %>% filter(upgrade,upgrade_cost > 1000) %>% group_by(simulation,grant_type,date) %>% summarise(n=n()) %>% mutate(n=cumsum(n)) %>% ggplot(aes(date,n,group=interaction(simulation,grant_type),colour=grant_type))+geom_line(alpha=alpha_0,linewidth=2)
g1 <- g1 + theme_minimal() + scale_colour_canva(palette="Sunny and calm") + theme(legend.position = "none")
g1 <- g1 + scale_y_continuous(limits=c(0,250))
abm  <- wam[[1]]
g2 <- abm %>% filter(upgrade,upgrade_cost > 1000) %>% group_by(simulation,grant_type,date) %>% summarise(n=n()) %>% mutate(n=cumsum(n)) %>% ggplot(aes(date,n,group=interaction(simulation,grant_type),colour=grant_type))+geom_line(alpha=alpha_0,linewidth=2)
g2 <- g2 + theme_minimal() + scale_colour_canva(palette="Sunny and calm") + theme(legend.position = "none")
g2 <- g2 + scale_y_continuous(limits=c(0,250))
abm  <- cap[[1]]
g3 <- abm %>% filter(upgrade,upgrade_cost > 1000) %>% group_by(simulation,grant_type,date) %>% summarise(n=n()) %>% mutate(n=cumsum(n)) %>% ggplot(aes(date,n,group=interaction(simulation,grant_type),colour=grant_type))+geom_line(alpha=alpha_0,linewidth=2)
g3 <- g3 + theme_minimal() + scale_colour_canva(palette="Sunny and calm") + theme(legend.position = "none")
g3 <- g3 + scale_y_continuous(limits=c(0,250))
abm  <- dwit[[1]]
g4 <- abm %>% filter(upgrade,upgrade_cost > 1000) %>% group_by(simulation,grant_type,date) %>% summarise(n=n()) %>% mutate(n=cumsum(n)) %>% ggplot(aes(date,n,group=interaction(simulation,grant_type),colour=grant_type))+geom_line(alpha=1,linewidth=2)
g4 <- g4 + theme_minimal() + scale_colour_canva(palette="Sunny and calm") #+ theme(legend.position = "none")
g4 <- g4 + scale_y_continuous(limits=c(0,250))

g1+g2+g3+g4
#export::graph2ppt(g1+g2+g3+g4,"~/Policy/CAMG/EED/Heat/grant_schemes.ppt")

#upgrades with heat pump
abm %>% filter(upgrade,tech=="heat_pump") %>% group_by(simulation,grant_type,date) %>% summarise(n=n()) %>% mutate(n=cumsum(n)) %>% ggplot(aes(date,n,group=interaction(simulation,grant_type),colour=grant_type))+geom_line()
#mean grant amounts
upgrades <- abm %>% filter(upgrade) %>% mutate(grant_share=upgrade_grant/upgrade_cost)
upgrades <- upgrades %>% group_by(grant_type,year=year(date)) %>% summarise(upgrade_cost=mean(upgrade_cost),upgrade_grant=mean(upgrade_grant), grant_share=mean(grant_share)) #%>% ggplot(aes(year,upgrda))
#
upgrades %>% ggplot(aes(year,grant_share,colour=grant_type))+geom_line()
#################
#cumulative heat pump funding
#######################
abm <- wem[[1]]
grants_cumulative <- abm %>% filter(tech_cost > 0, tech=="heat_pump") %>% group_by(grant_type,year=year(date)) %>% summarise(tech_cost=sum(tech_cost)/n_run,heat_pump_grant=sum(heat_pump_grant)/n_run)
grants_cumulative <- grants_cumulative %>% group_by(grant_type) %>% mutate(tech_cost=cumsum(tech_cost)/800*housing_stock_oo/1e+6,heat_pump_grant=cumsum(heat_pump_grant)/800*housing_stock_oo/1e+6)
#
grants_cumulative <- grants_cumulative %>% pivot_longer(cols=c(tech_cost,heat_pump_grant), values_to="Meuro",names_to="component")
#
grants_cumulative$scenario <- "WEM"
grants_wem <-  grants_cumulative

abm <- wam[[1]]
grants_cumulative <- abm %>% filter(tech_cost > 0, tech=="heat_pump") %>% group_by(grant_type,year=year(date)) %>% summarise(tech_cost=sum(tech_cost)/n_run,heat_pump_grant=sum(heat_pump_grant)/n_run)
grants_cumulative <- grants_cumulative %>% group_by(grant_type) %>% mutate(tech_cost=cumsum(tech_cost)/800*housing_stock_oo/1e+6,heat_pump_grant=cumsum(heat_pump_grant)/800*housing_stock_oo/1e+6)
#
grants_cumulative <- grants_cumulative %>% pivot_longer(cols=c(tech_cost,heat_pump_grant), values_to="Meuro",names_to="component")
#
grants_cumulative$scenario <- "WAM"
grants_wam <-  grants_cumulative

abm <- cap[[1]]
grants_cumulative <- abm %>% filter(tech_cost > 0, tech=="heat_pump") %>% group_by(grant_type,year=year(date)) %>% summarise(tech_cost=sum(tech_cost)/n_run,heat_pump_grant=sum(heat_pump_grant)/n_run)
grants_cumulative <- grants_cumulative %>% group_by(grant_type) %>% mutate(tech_cost=cumsum(tech_cost)/800*housing_stock_oo/1e+6,heat_pump_grant=cumsum(heat_pump_grant)/800*housing_stock_oo/1e+6)
#
grants_cumulative <- grants_cumulative %>% pivot_longer(cols=c(tech_cost,heat_pump_grant), values_to="Meuro",names_to="component")
#
grants_cumulative$scenario <- "CAP"
grants_cap <-  grants_cumulative

abm <- dwit[[1]]
grants_cumulative <- abm %>% filter(tech_cost > 0, tech=="heat_pump") %>% group_by(grant_type,year=year(date)) %>% summarise(tech_cost=sum(tech_cost)/n_run,heat_pump_grant=sum(heat_pump_grant)/n_run)
grants_cumulative <- grants_cumulative %>% group_by(grant_type) %>% mutate(tech_cost=cumsum(tech_cost)/800*housing_stock_oo/1e+6,heat_pump_grant=cumsum(heat_pump_grant)/800*housing_stock_oo/1e+6)
#
grants_cumulative <- grants_cumulative %>% pivot_longer(cols=c(tech_cost,heat_pump_grant), values_to="Meuro",names_to="component")
#
grants_cumulative$scenario <- "DWIT"
grants_dwit <-  grants_cumulative

grants <- bind_rows(grants_wem,grants_wam,grants_cap,grants_dwit)
grants$scenario <- factor(grants$scenario, levels=c("WEM","WAM","CAP","DWIT"))
#write_csv(grants,"~/Policy/CAMG/EED/Heat/data/cumulative_grants.csv")
g1 <- grants %>% filter(component=="heat_pump_grant") %>% ggplot(aes(year,Meuro/1000,fill=grant_type)) + geom_area()
g1 <- g1 + theme_minimal() + scale_fill_canva(palette="Cool jewel tones") + facet_wrap(.~scenario,nrow=1)
g1 <- g1 + scale_y_continuous(limits=c(0,15)) + scale_x_continuous(limits=c(2015,2040))
#export::graph2ppt(g1,"~/Policy/CAMG/EED/Heat/grant_scheme_hps_euros.ppt")
#grant table
#cumulative costs from 2021
test <- grants %>% filter(year %in% c(2020,2025,2030,2040)) %>% filter(component=="upgrade_grant") %>% select(-component) %>% pivot_wider(names_from="grant_type",values_from="Meuro")
#test <- test[,c(2,1,3,4,5)]
#test %>% filter(year==2020)
test <- test %>% mutate(BetterEnergyHomes=BetterEnergyHomes-170,WarmerHomes=WarmerHomes-175)
test$total <- test$BetterEnergyHomes + test$OSS + test$WarmerHomes
test <- test %>% filter(year != 2020) %>% arrange(year)
xt <- xtable(
  test,
  caption = "Projected Grant Funding",
  label = "tab:fec",
  digits=0,
  align = c("l", "l","l", "r","r","r","r")
)

print(
  xt,
  include.rownames = FALSE,
  booktabs = TRUE,
  sanitize.text.function = identity,
  caption.placement = "top"
)
######################################
# cumulative fabric upgrade grants
#####################################
abm <- wem[[1]]
grants_cumulative <- abm %>% filter(upgrade, grant_type != "None") %>% group_by(grant_type,year=year(date)) %>% summarise(upgrade_cost=sum(upgrade_cost)/n_run,upgrade_grant=sum(upgrade_grant)/n_run)
grants_cumulative <- grants_cumulative %>% group_by(grant_type) %>% mutate(upgrade_cost=cumsum(upgrade_cost)/800*housing_stock_oo/1e+6,upgrade_grant=cumsum(upgrade_grant)/800*housing_stock_oo/1e+6)
#
grants_cumulative <- grants_cumulative %>% pivot_longer(cols=c(upgrade_cost,upgrade_grant), values_to="Meuro",names_to="component")
#
grants_cumulative$scenario <- "WEM"
grants_wem <-  grants_cumulative

abm <- wam[[1]]
grants_cumulative <- abm %>% filter(upgrade, grant_type != "None") %>% group_by(grant_type,year=year(date)) %>% summarise(upgrade_cost=sum(upgrade_cost)/n_run,upgrade_grant=sum(upgrade_grant)/n_run)
grants_cumulative <- grants_cumulative %>% group_by(grant_type) %>% mutate(upgrade_cost=cumsum(upgrade_cost)/800*housing_stock_oo/1e+6,upgrade_grant=cumsum(upgrade_grant)/800*housing_stock_oo/1e+6)
#
grants_cumulative <- grants_cumulative %>% pivot_longer(cols=c(upgrade_cost,upgrade_grant), values_to="Meuro",names_to="component")
#
grants_cumulative$scenario <- "WAM"
grants_wam <- grants_cumulative

abm <- cap[[1]]
grants_cumulative <- abm %>% filter(upgrade, grant_type != "None") %>% group_by(grant_type,year=year(date)) %>% summarise(upgrade_cost=sum(upgrade_cost)/n_run,upgrade_grant=sum(upgrade_grant)/n_run)
grants_cumulative <- grants_cumulative %>% group_by(grant_type) %>% mutate(upgrade_cost=cumsum(upgrade_cost)/800*housing_stock_oo/1e+6,upgrade_grant=cumsum(upgrade_grant)/800*housing_stock_oo/1e+6)
#
grants_cumulative <- grants_cumulative %>% pivot_longer(cols=c(upgrade_cost,upgrade_grant), values_to="Meuro",names_to="component")
#
grants_cumulative$scenario <- "CAP"
grants_cap <-  grants_cumulative

abm <- dwit[[1]]
grants_cumulative <- abm %>% filter(upgrade, grant_type != "None") %>% group_by(grant_type,year=year(date)) %>% summarise(upgrade_cost=sum(upgrade_cost)/n_run,upgrade_grant=sum(upgrade_grant)/n_run)
grants_cumulative <- grants_cumulative %>% group_by(grant_type) %>% mutate(upgrade_cost=cumsum(upgrade_cost)/800*housing_stock_oo/1e+6,upgrade_grant=cumsum(upgrade_grant)/800*housing_stock_oo/1e+6)
#
grants_cumulative <- grants_cumulative %>% pivot_longer(cols=c(upgrade_cost,upgrade_grant), values_to="Meuro",names_to="component")
#
grants_cumulative$scenario <- "DWIT"
grants_dwit <-  grants_cumulative

grants <- bind_rows(grants_wem,grants_wam,grants_cap,grants_dwit)
grants$scenario <- factor(grants$scenario, levels=c("WEM","WAM","CAP","DWIT"))
#write_csv(grants,"~/Policy/CAMG/EED/Heat/data/cumulative_grants.csv")
g1 <- grants %>% filter(component=="upgrade_grant") %>% ggplot(aes(year,Meuro/1000,fill=grant_type)) + geom_area()
g1 <- g1 + theme_minimal() + scale_fill_canva(palette="Cool jewel tones") + facet_wrap(.~scenario,nrow=1)
g1 <- g1 + scale_y_continuous(limits=c(0,15)) + scale_x_continuous(limits=c(2015,2040))
#export::graph2ppt(g1,"~/Policy/CAMG/EED/Heat/grant_scheme_upgrades_euros.ppt")

#MAKE A COMBINED TABLE OF TOTAL COSTS
#grants_upgrades <- grants %>% filter(component=="upgrade_grant") %>% rename("upgrades"=Meuro) %>% select(-component)
#grants_hp <- grants %>% filter(component=="heat_pump_grant") %>% rename("heat_pumps"=Meuro) %>% select(-component) %>% filter(grant_type != "None")

grants_tab <- grants_upgrades %>% left_join(grants_hp) %>% mutate(heat_pumps=replace_na(heat_pumps,0)) %>% mutate(total_grant=heat_pumps+upgrades)

test <- grants_tab %>% group_by(scenario,year) %>% summarise(total=sum(total_grant)/1000) %>% pivot_wider(names_from=scenario,values_from=total)

test1 <- test %>% filter(year %in% c(2025,2030,2035,2040))

xt <- xtable(
  test1,
  caption = "Projected total grant funding since 2015",
  label = "tab:fec",
  digits=2,
  align = c("l", "l","r", "r","r","r")
)

print(
  xt,
  include.rownames = FALSE,
  booktabs = TRUE,
  sanitize.text.function = identity,
  caption.placement = "top"
)


#tax revenues

carbon_tax <- co2_all_mean %>% mutate(carbon=carbon_price_fun(sD,year+0.5))
carbon_tax <- carbon_tax %>% mutate(carbon=ifelse(scenario!="DWIT",carbon,carbon_price_fun(sD_dwit,year+0.5)))
carbon_tax <- carbon_tax %>% mutate(ct=(Mtco2_old+Mtco2_new)*carbon)
#cumulative carbon tax vs 2021
carbon_tax <- carbon_tax %>% filter(year > 2020) %>% group_by(scenario) %>% mutate(ct_cumul=cumsum(ct))
carbon_tax %>% ggplot(aes(year,ct_cumul,colour=scenario))+geom_line()

#combine with grant funding
funding <- grants_tab %>% group_by(year,scenario) %>% summarise(grant_cumul=sum(total_grant))
funding %>% filter(year==2020)
funding <- funding %>% filter(year > 2020) %>% mutate(grant_cumul=-(grant_cumul-345))
exchequer <- funding %>% inner_join(carbon_tax %>% select(year,scenario,ct_cumul))

exchequer <- exchequer %>% pivot_longer(cols=c(-scenario,-year),names_to="source",values_to="Meuro")
g <- exchequer %>% ggplot(aes(year,Meuro,fill=source)) + geom_area() +facet_wrap(.~scenario,nrow=1) + theme_minimal()
g <- g + scale_fill_canva(palette="Pop art")
#export::graph2ppt(g,"~/Policy/CAMG/EED/Heat/exchequer.ppt")

#write_csv(exchequer,"~/Policy/CAMG/EED/Heat/data/exchequer.csv")
test <- exchequer %>% group_by(scenario,year) %>% summarise(total=sum(Meuro)/1000)
#write_csv(test,"~/Policy/CAMG/EED/Heat/data/cba.csv")
test1 <- test %>% filter(year %in% c(2025,2030,2035,2040)) %>% pivot_wider(names_from=scenario,values_from=total)

xt <- xtable(
  test1,
  caption = "Projected cumulative cost of grants less carbon tax revenues",
  label = "tab:fec",
  digits=1,
  align = c("l", "l","r", "r","r","r")
)

print(
  xt,
  include.rownames = FALSE,
  booktabs = TRUE,
  sanitize.text.function = identity,
  caption.placement = "top"
)


abm <- cap[[1]]
tax <- abm %>% filter(upgrade, grant_type != "None") %>% group_by(grant_type,year=year(date)) %>% summarise(upgrade_cost=sum(upgrade_cost)/n_run,upgrade_grant=sum(upgrade_grant)/n_run)
grants_cumulative <- grants_cumulative %>% group_by(grant_type) %>% mutate(upgrade_cost=cumsum(upgrade_cost)/800*housing_stock_oo/1e+6,upgrade_grant=cumsum(upgrade_grant)/800*housing_stock_oo/1e+6)
#
grants_cumulative <- grants_cumulative %>% pivot_longer(cols=c(upgrade_cost,upgrade_grant), values_to="Meuro",names_to="component")
#
grants_cumulative$scenario <- "CAP"
grants_cap <-  grants_cumulative




grants_cumulative %>% ggplot(aes(year,Meuro,colour=grant_type,linetype=component))+geom_point()
#number of grants
abm %>% filter(upgrade) %>%
#num er of heat pumps by scheme
abm %>% filter(!is.na(heat_pump_grant)) %>% pull(grant_type) %>% table()
#mean grant sizes
grant_sizes <- abm %>% filter(upgrade > 0 | heat_pump_grant > 0, grant_type != "None") %>% group_by(grant_type,year=year(date)) %>% summarise(mean_grant=mean(upgrade_grant+heat_pump_grant,na.rm=T))
grant_sizes %>% ggplot(aes(year,mean_grant,colour=grant_type))+geom_point()
#distributions of grant sizes (in a specific year range)
grant_sizes <- abm %>% filter(upgrade > 0 | heat_pump_grant > 0, grant_type != "None", year(date)==2025) #%>% group_by(simulation,grant_type,year=year(date)) %>% summarise(grant=upgrade_grant+heat_pump_grant,na.rm=T))
grant_sizes %>% ggplot(aes(upgrade_grant+heat_pump_grant,fill=grant_type))+geom_histogram(alpha=0.5)

#########################
# new housing stock
#######################
completions <- read_csv("~/Policy/CAMG/EED/Heat/data/housing_completion_projections.csv") #flow
#convert to new_stock
params <- scenario_params(sD,2025)
#2015 and later only
completions <- completions %>% filter(year > 2014) %>% mutate(space_heating_requirement_wem = wem*space_heating_requirement(hli,floor_area,rebound=0.4,params))
completions <- completions %>% filter(year > 2014) %>% mutate(space_heating_requirement_wam = wam*space_heating_requirement(hli,floor_area,rebound=0.4,params))

completions <- completions %>% rowwise() %>% mutate(efficiency_gas = heating_system_efficiency("gas", year+0.5), efficiency_hp=heating_system_efficiency("heat_pump", year+0.5))
completions <- completions %>% mutate(fec_wem=heat_pump_fraction*space_heating_requirement_wem/efficiency_hp + (1-heat_pump_fraction)*space_heating_requirement_wem/efficiency_gas)
completions <- completions %>% mutate(fec_wam=heat_pump_fraction*space_heating_requirement_wam/efficiency_hp + (1-heat_pump_fraction)*space_heating_requirement_wam/efficiency_gas)
completions <- completions %>% mutate(gco2_wam = (1-heat_pump_fraction)*space_heating_requirement_wam/efficiency_gas*229,
                                      gco2_wem = (1-heat_pump_fraction)*space_heating_requirement_wem/efficiency_gas*229)

#TWh
completions <- completions %>% ungroup() %>% mutate(space_heating_requirement_wem = cumsum(space_heating_requirement_wem)/1e+9,fec_wem=cumsum(fec_wem/1e+9),
                                                    space_heating_requirement_wam = cumsum(space_heating_requirement_wam)/1e+9,fec_wam=cumsum(fec_wam/1e+9))
completions <- completions %>% ungroup() %>% mutate(Mtco2_wem = cumsum(gco2_wem)/1e+12,
                                                    Mtco2_wam = cumsum(gco2_wam)/1e+12)

#CO2 gas emissions only
fec_new <- completions %>% mutate(wem=cumsum(wem),wam = cumsum(wam)) %>% select(-hli,-ber,-heat_pump_fraction,-floor_area,-efficiency_gas,-efficiency_hp)
#
fec_mean <- fec_mean %>% rename("fec_old"=fec_prebound)
fec_new <- fec_new %>% select(year,fec_wam) %>% rename("fec_new"=fec_wam)
fec_all <- fec_mean %>% inner_join(fec_new) %>% mutate(fec=fec_new+fec_old)
#
#write_csv(fec_all,"~/Policy/CAMG/EED/Heat/data/fec_combined.csv")
fec_all <- fec_all %>% select(-fec) %>% pivot_longer(cols=c(-year,-scenario),names_to="stock")

g <- fec_all %>% ggplot()+ geom_area(aes(year,value,fill=stock))
g <- g + theme_minimal() + scale_fill_canva() + facet_wrap(.~scenario,nrow=2)
eed <- tibble(x=2023,xend=2030,y=20.84712,yend=20.84712*(1-0.125)) #2030 target 18.2 TWh
g <- g + geom_segment(data=eed,aes(x=x,y=y,xend=xend,yend=yend), linewidth=1,colour="grey50") + geom_point(data=eed,aes(xend,yend),size=2)
g <- g + geom_hline(yintercept = 18.24123, linetype="dotted") #+ scale_y_continuous(trans="sqrt")
g <- g + coord_cartesian(ylim=c(0,25)) + theme(axis.text.x = element_text(angle=-90))
g#
#export::graph2ppt(g,"~/Policy/CAMG/EED/Heat/fec_target.ppt")

test1 <- fec_mean %>% filter(year %in% c(2023,2030,2035,2040)) %>% pivot_wider(values_from=fec_old,names_from=scenario)
test <- test1 %>% inner_join(fec_new %>% filter(year %in% c(2023,2030,2035,2040)))

xt <- xtable(
  test,
  caption = "Projected FEC for pre-2015 and post-2015 housing stock",
  label = "tab:fec",
  digits=1,
  align = c("l", "r","r", "r","r","r","r","r")
)

print(
  xt,
  include.rownames = FALSE,
  booktabs = TRUE,
  sanitize.text.function = identity,
  caption.placement = "top"
)


#FEC target -11.8% below 2022 value

########################################
# public vs private investment
########################################
abm <- wam[[1]]
invest <- abm %>% group_by(simulation,date) %>% summarise(upgrade_investment=sum(upgrade_cost,na.rm=T), tech_investment=sum(tech_cost,na.rm=T),
                                                          upgrade_grants=sum(upgrade_grant,na.rm=T), heat_pump_grants=sum(heat_pump_grant,na.rm=T))

invest <- invest %>% group_by(date) %>% summarise(upgrade_investment=mean(upgrade_investment), tech_investment=mean(tech_investment),
                                                  upgrade_grants=mean(upgrade_grants),heat_pump_grants=mean(heat_pump_grants))
#
invest <- invest %>% mutate(upgrade_investment = cumsum(upgrade_investment), tech_investment = cumsum(tech_investment),
                            upgrade_grants=cumsum(upgrade_grants),heat_pump_grants=cumsum(heat_pump_grants))
invest <- invest %>% mutate(private_upgrade_investment=upgrade_investment-upgrade_grants,private_tech_investment=tech_investment-heat_pump_grants)
invest <- invest %>% select(-upgrade_investment,-tech_investment)
invest <- invest %>% pivot_longer(cols=-date)
g1 <- invest %>% filter(str_detect(name,"upgrade")) %>% ggplot(aes(date,value/800*housing_stock_oo/1e+9,fill=name)) + geom_area() + theme_minimal() + scale_fill_canva(palette = "Fun and cheerful") +
  scale_y_continuous(limits=c(0,4))
g2 <- invest %>% filter(!str_detect(name,"upgrade")) %>%
  ggplot(aes(date,value/800*housing_stock_oo/1e+9,fill=name)) + scale_y_continuous(limits=c(0,16)) + geom_area() + theme_minimal() + scale_fill_manual(values = rev(canva_pal("Fun and cheerful")(2)))
g1 /g2
export::graph2ppt(g1/g2,"~/Policy/CAMG/EED/Heat/wam_investment.ppt")
#

#############################
# cost benefit relative to WEM
###########################


##########################
# TIMES Ireland
#########################

scen_files <- list.files("~/Policy/CAMG/EU_2040_Targets/Post_2030_frameworks/CarbonBudgetScenariosforCCACCBWG2024-main/")
ffi_scens <- c("250mt-led","300mt-led","350mt-led","350mt-wam","350mt-wem","450mt-wam")
scen_files <- paste("mitigation_cb2024-",ffi_scens,".csv",sep="")
dir <- "~/Policy/CAMG/EU_2040_Targets/Post_2030_frameworks/CarbonBudgetScenariosforCCACCBWG2024-main/"
tim <- tibble()
for(scen in ffi_scens){
  filename <- paste("mitigation_cb2024-",scen,".csv",sep="")
  filename1 <- paste(dir,filename,sep="")
  file.exists(filename1)
  df <- read_csv(filename1) #%>% filter(tableName=="RSD_Services_CO2Emissions",seriesName=="RSD_BLD-XXX_NRGSRV-WS")#
  #df <- df %>% filter(tableName == "SYS_Emissions_CO2_Domestic", !(seriesName %in%  c("TRACO2INT")))
  df$scenario <- scen
  tim <- tim %>% bind_rows(df)
}

#emissions
co2_tim <- tim %>% filter(str_detect(label,"kt"), tableName=="RSD_Services_CO2Emissions")
co2_tim <- co2_tim %>% pivot_longer(cols=c(-tableName,-seriesName,-label,-scenario), names_to="year",values_to="kt")
co2_tim <- co2_tim %>% type_convert()

#FEC

test <- tim %>% filter(str_detect(tableName, "RSD_FEC"), label == "PJ") %>% arrange(-`2025`)

test <- test %>% pivot_longer(cols=c(-tableName,-seriesName,-label,-scenario), names_to="year",values_to="PJ")
test <- test %>% type_convert()
test %>% filter(scenario=="350mt-wam") %>% ggplot(aes(year,PJ,colour=seriesName)) + geom_line()
#remove ambient heat
test <- test %>% filter(!str_detect(seriesName,"AHT"))
#sum it up for FEC
fec_scen <- test %>% group_by(scenario,year) %>% summarise(fec=sum(PJ)*0.2778)
#write_csv(fec_scen,"~/Policy/CAMG/EED/Heat/data/fec_rsd_times_scenarios.csv")
fec_scen %>% ggplot(aes(year,fec,colour=scenario))+ geom_line()

################
# costs example
################
costs_4.5 <- tibble(hli_old=4.5,hli=seq(4.5,1.5,by=-0.1)) %>% mutate(cost=retrofit_cost_model(4.5,hli,"semi_detached",2,region="Munster",125,cost_model = "logistic",params))

costs_3.5 <- tibble(hli_old=3.5,hli=seq(3.5,1.5,by=-0.1)) %>% mutate(cost=retrofit_cost_model(3.5,hli,"semi_detached",2,region="Munster",125,cost_model = "logistic",params))

costs_2.5 <- tibble(hli_old=2.5,hli=seq(2.5,1.5,by=-0.1)) %>% mutate(cost=retrofit_cost_model(2.5,hli,"semi_detached",2,region="Munster",125,cost_model = "logistic",params))

costs <- bind_rows(costs_2.5,costs_3.5,costs_4.5)
g <- costs %>% ggplot(aes(hli,cost,colour=factor(hli_old)))+geom_line(size=1.5)+theme_minimal() + scale_colour_canva(palette="Simple but bold")
#export::graph2ppt(g,"~/Policy/CAMG/EED/Heat/upgrade_costs1.ppt")


############################
# who's paying carbon tax?
_###############################

emissions_factors <- read_csv("C:/Users/Joe/pkgs/hpmicrosimr/inst/extdata/tech_emissions_factors.csv")

abm <- wem[[1]]
abm <- abm %>% mutate(income_tercile = case_when(income < 37500~"low",income > 62400~"high",(income <= 62400) & (income >= 37500)~"middle"))
abm <- abm %>% mutate(space_heating_requirement_actual = space_heating_requirement(hli,floor_area,rebound=0.4,params0))
#abm <- abm %>% mutate(temperature_deficit = (space_heating_requirement_theory-space_heating_requirement_actual)/(8.76*hli*floor_area))
abm <- abm %>% mutate(efficiency = heating_system_efficiency(tech,heating_install_time))
abm <- abm %>% mutate(fec=space_heating_requirement_actual/efficiency)
abm <- abm %>% inner_join(emissions_factors)
abm <- abm %>% mutate(tco2=gCO2_per_kWh*fec/1e+6)
#
df <- abm %>% group_by(simulation,income_tercile,date) %>% summarise(tco2=sum(tco2)) %>% inner_join(df0) %>% mutate(Mtco2=tco2/n0*housing_stock_oo/1e+6)
df <- df %>% select(-n0,-tco2)
df <- df %>% group_by(income_tercile,year=year(date)) %>% summarise(Mtco2=mean(Mtco2))
#annual
#df <- df %>% group_by(scenario,income_tercile,year=year(date)) %>% summarise(Mtco2=mean(Mtco2))
df$scenario <- "WEM"
df_wem <- df

abm <- wam[[1]]
abm <- abm %>% mutate(income_tercile = case_when(income < 37500~"low",income > 62400~"high",(income <= 62400) & (income >= 37500)~"middle"))
abm <- abm %>% mutate(space_heating_requirement_actual = space_heating_requirement(hli,floor_area,rebound=0.4,params0))
#abm <- abm %>% mutate(temperature_deficit = (space_heating_requirement_theory-space_heating_requirement_actual)/(8.76*hli*floor_area))
abm <- abm %>% mutate(efficiency = heating_system_efficiency(tech,heating_install_time))
abm <- abm %>% mutate(fec=space_heating_requirement_actual/efficiency)
abm <- abm %>% inner_join(emissions_factors)
abm <- abm %>% mutate(tco2=gCO2_per_kWh*fec/1e+6)
#
df <- abm %>% group_by(simulation,income_tercile,date) %>% summarise(tco2=sum(tco2)) %>% inner_join(df0) %>% mutate(Mtco2=tco2/n0*housing_stock_oo/1e+6)
df <- df %>% select(-n0,-tco2)
df <- df %>% group_by(income_tercile,year=year(date)) %>% summarise(Mtco2=mean(Mtco2))
#annual
df$scenario <- "WAM"
df_wam <- df

abm <- cap[[1]]
abm <- abm %>% mutate(income_tercile = case_when(income < 37500~"low",income > 62400~"high",(income <= 62400) & (income >= 37500)~"middle"))
abm <- abm %>% mutate(space_heating_requirement_actual = space_heating_requirement(hli,floor_area,rebound=0.4,params0))
#abm <- abm %>% mutate(temperature_deficit = (space_heating_requirement_theory-space_heating_requirement_actual)/(8.76*hli*floor_area))
abm <- abm %>% mutate(efficiency = heating_system_efficiency(tech,heating_install_time))
abm <- abm %>% mutate(fec=space_heating_requirement_actual/efficiency)
abm <- abm %>% inner_join(emissions_factors)
abm <- abm %>% mutate(tco2=gCO2_per_kWh*fec/1e+6)
#
df <- abm %>% group_by(simulation,income_tercile,date) %>% summarise(tco2=sum(tco2)) %>% inner_join(df0) %>% mutate(Mtco2=tco2/n0*housing_stock_oo/1e+6)
df <- df %>% select(-n0,-tco2)
df <- df %>% group_by(income_tercile,year=year(date)) %>% summarise(Mtco2=mean(Mtco2))
#annual
df$scenario <- "CAP"
df_cap <- df

abm <- dwit[[1]]
abm <- abm %>% mutate(income_tercile = case_when(income < 37500~"low",income > 62400~"high",(income <= 62400) & (income >= 37500)~"middle"))
abm <- abm %>% mutate(space_heating_requirement_actual = space_heating_requirement(hli,floor_area,rebound=0.4,params0))
#abm <- abm %>% mutate(temperature_deficit = (space_heating_requirement_theory-space_heating_requirement_actual)/(8.76*hli*floor_area))
abm <- abm %>% mutate(efficiency = heating_system_efficiency(tech,heating_install_time))
abm <- abm %>% mutate(fec=space_heating_requirement_actual/efficiency)
abm <- abm %>% inner_join(emissions_factors)
abm <- abm %>% mutate(tco2=gCO2_per_kWh*fec/1e+6)
#
df <- abm %>% group_by(simulation,income_tercile,date) %>% summarise(tco2=sum(tco2)) %>% inner_join(df0) %>% mutate(Mtco2=tco2/n0*housing_stock_oo/1e+6)
df <- df %>% select(-n0,-tco2)
df <- df %>% group_by(income_tercile,year=year(date)) %>% summarise(Mtco2=mean(Mtco2))
#annual
df$scenario <- "DWIT"
df_dwit <- df
#
df_dwit %>% ggplot(aes(year,Mtco2,colour=income_tercile))+geom_line()
co2_income <- bind_rows(df_wem,df_wam,df_cap,df_dwit)
co2_income$scenario <- factor(co2_income$scenario,levels=c("WEM","WAM","CAP","DWIT"))
co2_income %>% ggplot(aes(year,Mtco2,colour=income_tercile))+geom_line() + facet_wrap(.~scenario)

#total grant funding by income tercile

abm <- wem[[1]]
abm <- abm %>% mutate(income_tercile = case_when(income < 37500~"low",income > 62400~"high",(income <= 62400) & (income >= 37500)~"middle"))
grants_income <- abm %>% filter(grant_type %in% c("WarmerHomes","BetterEnergyHomes","OSS")) %>% group_by(income_tercile,year=year(date)) %>% summarise(grant=sum(upgrade_grant+heat_pump_grant)/n_run)
grants_income_cumul <- grants_income %>% group_by(income_tercile) %>% mutate(grant=cumsum(grant)/800*housing_stock_oo/1e+6)
#
#grants_cumulative <- grants_cumulative %>% pivot_longer(cols=c(tech_cost,heat_pump_grant), values_to="Meuro",names_to="component")
#
grants_income_cumul$scenario <- "WEM"
grants_income_wem <-  grants_income_cumul

abm <- wam[[1]]
abm <- abm %>% mutate(income_tercile = case_when(income < 37500~"low",income > 62400~"high",(income <= 62400) & (income >= 37500)~"middle"))
grants_income <- abm %>% filter(grant_type %in% c("WarmerHomes","BetterEnergyHomes","OSS")) %>% group_by(income_tercile,year=year(date)) %>% summarise(grant=sum(upgrade_grant+heat_pump_grant)/n_run)
grants_income_cumul <- grants_income %>% group_by(income_tercile) %>% mutate(grant=cumsum(grant)/800*housing_stock_oo/1e+6)
#
#grants_cumulative <- grants_cumulative %>% pivot_longer(cols=c(tech_cost,heat_pump_grant), values_to="Meuro",names_to="component")
#
grants_income_cumul$scenario <- "WAM"
grants_income_wam <-  grants_income_cumul

abm <- cap[[1]]
abm <- abm %>% mutate(income_tercile = case_when(income < 37500~"low",income > 62400~"high",(income <= 62400) & (income >= 37500)~"middle"))
grants_income <- abm %>% filter(grant_type %in% c("WarmerHomes","BetterEnergyHomes","OSS")) %>% group_by(income_tercile,year=year(date)) %>% summarise(grant=sum(upgrade_grant+heat_pump_grant)/n_run)
grants_income_cumul <- grants_income %>% group_by(income_tercile) %>% mutate(grant=cumsum(grant)/800*housing_stock_oo/1e+6)
#
#grants_cumulative <- grants_cumulative %>% pivot_longer(cols=c(tech_cost,heat_pump_grant), values_to="Meuro",names_to="component")
#
grants_income_cumul$scenario <- "CAP"
grants_income_cap <-  grants_income_cumul

abm <- dwit[[1]]
abm <- abm %>% mutate(income_tercile = case_when(income < 37500~"low",income > 62400~"high",(income <= 62400) & (income >= 37500)~"middle"))
grants_income <- abm %>% filter(grant_type %in% c("WarmerHomes","BetterEnergyHomes","OSS")) %>% group_by(income_tercile,year=year(date)) %>% summarise(grant=sum(upgrade_grant+heat_pump_grant)/n_run)
grants_income_cumul <- grants_income %>% group_by(income_tercile) %>% mutate(grant=cumsum(grant)/800*housing_stock_oo/1e+6)
#
#grants_cumulative <- grants_cumulative %>% pivot_longer(cols=c(tech_cost,heat_pump_grant), values_to="Meuro",names_to="component")
#
grants_income_cumul$scenario <- "DWIT"
grants_income_dwit <-  grants_income_cumul

grants_income <- bind_rows(grants_income_wem,grants_income_wam,grants_income_cap,grants_income_dwit)
grants_income$scenario <- factor(grants_income$scenario, levels=c("WEM","WAM","CAP","DWIT"))

carbon_tax_income <- co2_income %>% mutate(carbon=carbon_price_fun(sD,year+0.5))
carbon_tax_income <- carbon_tax_income %>% mutate(carbon=ifelse(scenario!="DWIT",carbon,carbon_price_fun(sD_dwit,year+0.5)))

carbon_tax_income <- carbon_tax_income %>% mutate(ct=carbon*Mtco2) %>% select(scenario,income_tercile,year,ct)
#cumulative
carbon_tax_income <- carbon_tax_income %>% group_by(scenario,income_tercile) %>% mutate(ct=cumsum(ct))


distrib_income <- grants_income %>% inner_join(carbon_tax_income)

distrib_income <- distrib_income %>% mutate(net=grant-ct)
#from 2021
distrib_income %>% filter(year==2020)
distrib_income$income_tercile <- factor(distrib_income$income_tercile,levels=c("low","middle","high"))
distrib_income<- distrib_income %>% filter(year >=2020) %>% mutate(net = net - first(net)) %>% filter(year > 2020)
g <- distrib_income %>% ggplot(aes(year,net,colour=income_tercile))+geom_line(linewidth=1.25) + facet_wrap(.~scenario,nrow=1)
g <- g + theme_minimal() + scale_colour_canva(palette="Fun and cheerful")
g
#export::graph2ppt(g,"~/Policy/CAMG/EED/Heat/distributional.ppt")
distrib_income %>% filter(year %in% c(2030,2040)) %>% select(-grant,-ct) %>% pivot_wider(names_from=scenario,values_from=net)


#abm <- wem[[1]]
params0 <- scenario_params(sD,2025)
abm <- dwit[[1]]
abm <- abm %>% mutate(space_heating_requirement_actual = space_heating_requirement(hli,floor_area,rebound=0.4,params0))
abm <- abm %>% mutate(space_heating_requirement_theory = space_heating_requirement(hli,floor_area,rebound=0,params0))
abm <- abm %>% mutate(temperature_deficit = (space_heating_requirement_theory-space_heating_requirement_actual)/(8.76*hli*floor_area))


abm <- abm %>% mutate(efficiency = heating_system_efficiency(tech,heating_install_time))
df <- abm %>% group_by(tech,date) %>% summarise(efficiency=mean(efficiency))
#df %>% ggplot(aes(date,efficiency,colour=tech))+geom_line()
#FEC
abm <- abm %>% mutate(fec_prebound=space_heating_requirement_actual/efficiency, fec_theory=space_heating_requirement_theory/efficiency)
#totals
fec <- abm %>% group_by(simulation,date) %>% summarise(fec_prebound=sum(fec_prebound),fec_theory=sum(fec_theory)) %>% inner_join(df0)
fec <- fec %>% mutate(fec_prebound=fec_prebound/n0*housing_stock_oo, fec_theory=fec_theory/n0*housing_stock_oo)
fec <- fec %>% select(-n0) %>% group_by(simulation,year=year(date)) %>% summarise(fec_prebound=mean(fec_prebound),fec_theory=mean(fec_theory))
#g1 <- fec %>% ggplot(aes(year,fec_theory/10^9,colour=factor(simulation)))+geom_line() + theme_minimal() + theme(legend.position="none")
#g1 <- g1 + scale_y_continuous(limits=c(15,32))
fec <- fec %>% mutate(fec_prebound=fec_prebound/10^9)
fec_dwit <- fec
fec_dwit$scenario <- "DWIT"

fec <- bind_rows(fec_wem,fec_wam,fec_cap,fec_dwit)
fec$scenario <- factor(fec$scenario,levels=c("WEM","WAM","CAP","DWIT"))

fec_mean <- fec %>% group_by(scenario,year) %>% summarise(fec=mean(fec_prebound))

############################
# cost-benefit analysis
##########################

#fiscal costs
cba  <- read_csv("~/Policy/CAMG/EED/Heat/data/cba.csv")
cba <- cba %>% group_by(scenario) %>% mutate(cost=total-lag(total))
cba <- cba %>% mutate(cost=-cost) %>% filter(year> 2025, year != 2040) #remove 2040 glitch
cba <- cba %>% rename("co2"=Mtco2_old)

compliance <- tibble(year=c(2026,2030,2039),carbon_price=c(80,100,200)) %>% right_join(tibble(year=2026:2039)) %>% arrange(year)
#
compliance <- compliance %>% mutate(carbon_price = zoo::na.approx(carbon_price))
compliance <- compliance %>% bind_rows(tibble(year=2040:2100,carbon_price=0))
#
cba_aea <- tibble(year=c(2021,2030,2039),aea=c(4.23,(1-0.37)*4.23,(1-0.85)*4.23)) %>% right_join(tibble(year=2021:2039)) %>% arrange(year)
cba_aea <- cba_aea %>% mutate(aea = zoo::na.approx(aea))
#extend aea unchanged to 2100 (policy ends)
cba_aea <- cba_aea %>% bind_rows(tibble(year=2040:2100,aea=filter(cba_aea, year==2039) %>% pull(aea)))

cba <- cba %>% inner_join(compliance) %>% inner_join(cba_aea)
#compliance cost
cba <- cba %>% mutate(compliance = carbon_price*(co2-aea)/1000) #cost in Bn

#counterfactual = WEM
cba_wem <- cba %>% ungroup() %>% filter(scenario=="WEM") %>% select(year,fec,co2,cost,compliance)
cba_wem <- cba_wem %>% rename("fec_wem"=fec,"co2_wem"=co2, "cost_wem"=cost,"compliance_wem"=compliance)
#
cba <- cba %>% filter(scenario != "WEM") %>% select(-total)
cba <- cba %>% inner_join(cba_wem)
#abatement
cba <- cba %>% mutate(fec_abate = fec_wem-fec, co2_abate=co2_wem-co2,cost_add=cost-cost_wem, compliance_add=compliance-compliance_wem)
cba <- cba %>% select(!contains("wem"))

cba_ext <- expand_grid(scenario=c("WEM","WAM","CAP","DWIT"), year=2040:2100) %>% inner_join(cba %>% filter(year==2039) %>% select(-year))
#
cba_ext$cost_add <- 0
cba_ext$cost <- 0
cba_ext$compliance_add <- 0

cba <- cba %>% bind_rows(cba_ext)
cba <- cba %>% mutate(df=1/(1+0.03)^(year-2025))
cba <- cba %>% mutate(cost_disc= df*cost_add, fec_disc=df*fec_abate,co2_disc = co2_abate*df, compliance_disc=df*compliance_add)
# add compliance costs "moderate" scenario 100 in 2030 and 200 in 2040. current value in 90

cba_summary <- cba %>% group_by(scenario) %>% summarise(cost_disc_inc=sum(cost_disc+compliance_disc),cost_disc_ex=sum(cost_disc), fec_disc=sum(fec_disc), co2_disc=sum(co2_disc))

cba_summary$scenario <- factor(cba_summary$scenario,levels=c("WEM","WAM","CAP","DWIT"))

cba_summary %>% mutate(euros_per_MWh=cost_disc_inc*1e+9/(fec_disc*1e+6),euros_per_tco2=cost_disc_inc*1e+9/(co2_disc*1e+6),
                       euros_per_MWh_ex=cost_disc_ex*1e+9/(fec_disc*1e+6),euros_per_tco2_ex=cost_disc_ex*1e+9/(co2_disc*1e+6) ) %>% arrange(scenario)

test <- cba_summary %>% mutate(euro_per_MWh=cost_disc_ex*1e+9/(fec_disc*1e+6),euros_per_tco2=cost_disc_ex*1e+9/(co2_disc*1e+6)) %>% arrange(scenario)
test1 <- test %>% select(scenario,euro_per_MWh,euros_per_tco2)
#
write_csv(cba,"~/Policy/CAMG/EED/Heat/data/efficincy_cost_benefit.csv")


xt <- xtable(
  test1,
  caption = "Implied abatment cost with 4% discount rate excluding ESR compliance costs",
  label = "tab:fec",
  digits=0,
  align = c("l", "l", "r","r")
)

print(
  xt,
  include.rownames = FALSE,
  booktabs = TRUE,
  sanitize.text.function = identity,
  caption.placement = "top"
)

##################################################
# comparing ABM scenario outputs to TIMES Model
##################################################

fec_tim <- read_csv("~/Policy/CAMG/EED/Heat/data/fec_rsd_times_scenarios.csv")
fec_tim$model <- "TIMES"
#remove nn-space heating
0.4*31
#fec_tim <- fec_tim %>% mutate(fec=fec-12.4)
fec_abm <- read_csv("~/Policy/CAMG/EED/Heat/data/fec_combined.csv")
fec_abm$model <- "ABM"
fec_abm$scenario <- factor(fec_abm$scenario,levels=c("WEM","WAM","CAP","DWIT"))
fec_tim <- fec_tim %>% mutate(fec=fec-11)

fec_all <- fec_tim %>% bind_rows(fec_abm) %>% filter(year >= 2018, year <= 2040)
g1 <- fec_abm %>% ggplot(aes(year,fec,colour=scenario))+geom_line(linewidth=1.25) + theme_minimal() + scale_colour_canva(palette="Fun and cheerful")
g1 <- g1 + scale_y_continuous(limits=c(5,30))
g2 <- fec_all %>% filter(model=="TIMES") %>% ggplot(aes(year,fec,colour=scenario))+geom_line(linewidth=1.25) + theme_minimal() #+ scale_colour_canva()
g2 <- g2 + scale_y_continuous(limits=c(5,30))
g1+g2

#export::graph2ppt(g1+g2,"~/Policy/CAMG/EED/Heat/fec_least_cost_vs_abm2.ppt")

fec_tim %>% filter(year %in% c(2023,2030)) %>% pivot_wider(names_from=year,values_from=fec) %>% mutate(drop=100*(`2030`/`2023`-1))

fec_abm %>% filter(year %in% c(2023,2030)) %>% pivot_wider(names_from=year,values_from=fec) %>% mutate(drop=100*(`2030`/`2023`-1))


#################################
# energy bills
###################################

fuel_prices <- expand_grid(date=abm$date %>% unique(),tech=c("heat_pump","electricity","gas","oil","solid_fuel"))
fuel_prices <- fuel_prices %>% rowwise() %>% mutate(fuel_price=energy_price_fun(ifelse(tech=="heat_pump","electricity",tech),sD_wem,decimal_date(date)))
fuel_prices_dwit <- fuel_prices %>% rowwise() %>% mutate(fuel_price=energy_price_fun(ifelse(tech=="heat_pump","electricity",tech),sD_dwit,decimal_date(date)))


abm <- wem[[1]]
abm <- abm %>% inner_join(fuel_prices)
abm <- abm %>% mutate(income_tercile = case_when(income < 37500~"low",income > 62400~"high",(income <= 62400) & (income >= 37500)~"middle"))
#
abm <- abm %>% mutate(space_heating_requirement_actual = space_heating_requirement(hli,floor_area,rebound=0.4,params0))
abm <- abm %>% mutate(efficiency = heating_system_efficiency(tech,heating_install_time))

abm <- abm %>% mutate(bills=space_heating_requirement_actual/efficiency*fuel_price/100)

df <- abm %>% group_by(income_tercile,year=year(date)) %>% summarise(bills=mean(bills)) # %>% inner_join(df0) %>% mutate(Mtco2=tco2/n0*housing_stock_oo/1e+6)
#annual
#df <- df %>% group_by(scenario,income_tercile,year=year(date)) %>% summarise(Mtco2=mean(Mtco2))
df$scenario <- "WEM"
df_wem <- df

abm <- wam[[1]]
abm <- abm %>% inner_join(fuel_prices)
abm <- abm %>% mutate(income_tercile = case_when(income < 37500~"low",income > 62400~"high",(income <= 62400) & (income >= 37500)~"middle"))
#
abm <- abm %>% mutate(space_heating_requirement_actual = space_heating_requirement(hli,floor_area,rebound=0.4,params0))
abm <- abm %>% mutate(efficiency = heating_system_efficiency(tech,heating_install_time))

abm <- abm %>% mutate(bills=space_heating_requirement_actual/efficiency*fuel_price/100)

df <- abm %>% group_by(income_tercile,year=year(date)) %>% summarise(bills=mean(bills)) # %>% inner_join(df0) %>% mutate(Mtco2=tco2/n0*housing_stock_oo/1e+6)
#annual
#df <- df %>% group_by(scenario,income_tercile,year=year(date)) %>% summarise(Mtco2=mean(Mtco2))
df$scenario <- "WAM"
df_wam <- df

abm <- cap[[1]]
abm <- abm %>% inner_join(fuel_prices)
abm <- abm %>% mutate(income_tercile = case_when(income < 37500~"low",income > 62400~"high",(income <= 62400) & (income >= 37500)~"middle"))
#
abm <- abm %>% mutate(space_heating_requirement_actual = space_heating_requirement(hli,floor_area,rebound=0.4,params0))
abm <- abm %>% mutate(efficiency = heating_system_efficiency(tech,heating_install_time))

abm <- abm %>% mutate(bills=space_heating_requirement_actual/efficiency*fuel_price/100)

df <- abm %>% group_by(income_tercile,year=year(date)) %>% summarise(bills=mean(bills)) # %>% inner_join(df0) %>% mutate(Mtco2=tco2/n0*housing_stock_oo/1e+6)
#annual
#df <- df %>% group_by(scenario,income_tercile,year=year(date)) %>% summarise(Mtco2=mean(Mtco2))
df$scenario <- "CAP"
df_cap <- df

df_cap %>% ggplot(aes(year,bills,colour=income_tercile))+geom_line()


abm <- dwit[[1]]
abm <- abm %>% inner_join(fuel_prices_dwit)
abm <- abm %>% mutate(income_tercile = case_when(income < 37500~"low",income > 62400~"high",(income <= 62400) & (income >= 37500)~"middle"))
#
abm <- abm %>% mutate(space_heating_requirement_actual = space_heating_requirement(hli,floor_area,rebound=0.4,params0))
abm <- abm %>% mutate(efficiency = heating_system_efficiency(tech,heating_install_time))

abm <- abm %>% mutate(bills=space_heating_requirement_actual/efficiency*fuel_price/100)

df <- abm %>% group_by(income_tercile,year=year(date)) %>% summarise(bills=mean(bills)) # %>% inner_join(df0) %>% mutate(Mtco2=tco2/n0*housing_stock_oo/1e+6)
#annual
#df <- df %>% group_by(scenario,income_tercile,year=year(date)) %>% summarise(Mtco2=mean(Mtco2))
df$scenario <- "DWIT"
df_dwit <- df

bills <- bind_rows(df_wem,df_wam,df_cap,df_dwit)
bills$scenario <- factor(bills$scenario,levels=c("WEM","WAM","CAP","DWIT"))
bills$income_tercile <- factor(bills$income_tercile,levels=c("low","middle","high"))

g <- bills %>% ggplot(aes(year,bills,colour=income_tercile))+geom_line(linewidth=1.25) + theme_minimal() #+ theme(legend.position="none")
g <- g + scale_colour_canva(palette="Fun and cheerful")+ facet_wrap(.~scenario,nrow=1)
g
export::graph2ppt(g,"~/Policy/CAMG/EED/Heat/bills.ppt")
