#hpmicrosimr usage
library(hpmicrosimr)
sessionInfo()
library(tidyverse)

sD_wem <- readxl::read_xlsx("C:/Users/Joe/pkgs/hpmicrosimr/inst/extdata/scenario_parameters.xlsx",sheet="WEM")
sD_wam <- readxl::read_xlsx("C:/Users/Joe/pkgs/hpmicrosimr/inst/extdata/scenario_parameters.xlsx",sheet="WAM")

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

wam <- readRDS("~/Policy/CAMG/EED/Heat/data/wam_1.35.RData")
wem <- readRDS("~/Policy/CAMG/EED/Heat/data/wem.RData")
abm <- wam[[1]]
n_run <- wem[[3]] %>% filter(parameter=="Nrun") %>% pull(value)
df0 <- abm %>% group_by(simulation,date) %>% summarise(n0=n())
housing_stock_oo <- 1147552 #census

params <- scenario_params(wam[[2]],2025)
params$grant_increase_factor

test2 <- abm %>% group_by(simulation,date,tech) %>% summarise(n=n()) %>% inner_join(df0)
test2 <- test2 %>% mutate(n=n/n0*housing_stock_oo) %>% select(-n0)
test2 <- test2 %>% group_by(tech,date) %>% summarise(n=mean(n))
test2 <- test2 %>% ungroup() %>% arrange(date,factor(tech,levels = c("heat_pump","electricity","oil","gas","solid_fuel")))
test2 <- test2 %>% mutate(tech=factor(tech,levels=rev(c("heat_pump","electricity","oil","gas","solid_fuel"))))

g <- test2 %>% ggplot(aes(date,n,fill=tech))+geom_area()
g <- g + theme_minimal() + scale_fill_viridis_d()
g
#export::graph2ppt(g,"~/Policy/CAMG/EED/Heat/wem_uptake.ppt")
#annual year end number fo heat pumps
ntech <- test2 %>% filter(tech=="heat_pump",str_detect(date,"-01-01")) %>% mutate(year_end=year(date)) %>% select(-date)
#write_csv(ntech,"~/Policy/CAMG/EED/Heat/data/wem_ntech.csv")
ntech_wem <- read_csv("~/Policy/CAMG/EED/Heat/data/wem_ntech.csv")


#number of b2s = wem vs wam
abm <- wem[[1]]
nb2 <- abm %>% filter(ber <= 125) %>% group_by(simulation,date) %>% summarise(n_b2 = n()) %>% inner_join(df0)
nb2 <- nb2 %>% mutate(n_b2=n_b2/n0*housing_stock_oo) %>% select(-n0)
nb2 <- nb2 %>% group_by(date) %>% summarise(wem=mean(n_b2))
abm <- wam[[1]]
nb2_wam <- abm %>% filter(ber <= 125) %>% group_by(simulation,date) %>% summarise(n_b2 = n()) %>% inner_join(df0)
nb2_wam <- nb2_wam %>% mutate(n_b2=n_b2/n0*housing_stock_oo) %>% select(-n0)
nb2_wam <- nb2_wam %>% group_by(date) %>% summarise(wam=mean(n_b2))
nb2<- nb2 %>% inner_join(nb2_wam)
nb2 <- nb2 %>% pivot_longer(cols=-date)

g <- nb2 %>% ggplot(aes(date,value,colour=name))+geom_line() + theme_minimal() + theme(legend.position = "none")
g
export::graph2ppt(g,"~/Policy/CAMG/EED/Heat/nb2_wem_wam.ppt")

nb2 <- nb2 %>% group_by(date) %>% summarise(n_b2=mean(n_b2)) %>% filter(str_detect(date,"-01-01")) %>% mutate(year_end=year(date)) %>% select(-date)


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
abm <- wam[[1]]
abm <- abm %>% mutate(space_heating_requirement_actual = space_heating_requirement(hli,floor_area,rebound=0.4,params0))
abm <- abm %>% mutate(space_heating_requirement_theory = space_heating_requirement(hli,floor_area,rebound=0,params0))
abm <- abm %>% mutate(temperature_deficit = (space_heating_requirement_theory-space_heating_requirement_actual)/(8.76*hli*floor_area))

heat <- abm %>% group_by(simulation,date) %>% summarise(space_heating_requirement = sum(space_heating_requirement_theory))
heat %>% ggplot(aes(date,space_heating_requirement,colour=factor(simulation))) + geom_line() + theme(legend.position="none")

temperature <- abm %>% filter(income < 37500) %>% group_by(simulation,date) %>% summarise(degree_deficit = mean(temperature_deficit))
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

g2 <- fec %>% ggplot(aes(year,fec_prebound/10^9,colour=factor(simulation)))+geom_line() + theme_minimal() + theme(legend.position="none")
g2 <- g2 + scale_y_continuous(limits=c(15,25))
g1 + g2
#export::graph2ppt(g1+g2,"~/Policy/CAMG/EED/Heat/fec_wem_wam.ppt")

#EED
fec %>% filter(year==2022) %>% pull(fec_prebound) %>% mean()/1e+9 #20TWh
fec %>% filter(year==2030) %>% pull(fec_prebound) %>% mean()/1e+9 #17.9 TWh
#1-17.9/20 -10.5 vs -12.6% target - inadequate

fec %>% group_by(year) %>% summarise(fec_prebound=mean(fec_prebound)/1e+9,fec_theory=mean(fec_theory)/1e+9)

##################################
# co2 emissions
################################

emissions_factors <- read_csv("C:/Users/Joe/pkgs/hpmicrosimr/inst/extdata/tech_emissions_factors.csv")
abm <-wam[[1]]

abm <- abm %>% mutate(space_heating_requirement_actual = space_heating_requirement(hli,floor_area,rebound=0.4,params0))
abm <- abm %>% mutate(space_heating_requirement_theory = space_heating_requirement(hli,floor_area,rebound=0,params0))
abm <- abm %>% mutate(temperature_deficit = (space_heating_requirement_theory-space_heating_requirement_actual)/(8.76*hli*floor_area))

abm <- abm %>% inner_join(emissions_factors)
abm <- abm %>% mutate(tco2=gCO2_per_kWh*space_heating_requirement_actual/1e+6)
#
df <- abm %>% group_by(simulation,date) %>% summarise(tco2=sum(tco2)) %>% inner_join(df0) %>% mutate(Mtco2=tco2/n0*housing_stock_oo/1e+6)
#
g <- df %>% ggplot(aes(date,Mtco2,colour=factor(simulation)))+geom_line()
g <- g + theme_minimal() + theme(legend.position="none") + scale_y_continuous(limits=c(3,4.5))
g2 <- g
g1 + g2
#export::graph2ppt(g1+g2,"~/Policy/CAMG/EED/Heat/emissions_wem_wam.ppt")

###############################
# grants, upgrades, failures
###############################

abm %>% filter(failure) %>% group_by(simulation,date) %>% summarise(n=n()) %>% mutate(n=cumsum(n)) #%>% ggplot(aes(date,n,group=interaction(simulation),colour=)+geom_line()
#upgrades
abm %>% filter(upgrade) %>% group_by(simulation,grant_type,date) %>% summarise(n=n()) %>% mutate(n=cumsum(n)) %>% ggplot(aes(date,n,group=interaction(simulation,grant_type),colour=grant_type))+geom_line()
#upgrades with heat pump
abm %>% filter(upgrade,tech=="heat_pump") %>% group_by(simulation,grant_type,date) %>% summarise(n=n()) %>% mutate(n=cumsum(n)) %>% ggplot(aes(date,n,group=interaction(simulation,grant_type),colour=grant_type))+geom_line()
#mean grant amounts
upgrades <- abm %>% filter(upgrade) %>% mutate(grant_share=upgrade_grant/upgrade_cost)
upgrades <- upgrades %>% group_by(grant_type,year=year(date)) %>% summarise(upgrade_cost=mean(upgrade_cost),upgrade_grant=mean(upgrade_grant), grant_share=mean(grant_share)) #%>% ggplot(aes(year,upgrda))
#
upgrades %>% ggplot(aes(year,grant_share,colour=grant_type))+geom_line()
#cumulative costs
grants_cumulative <- abm %>% filter(upgrade, grant_type != "None") %>% group_by(grant_type,year=year(date)) %>% summarise(upgrade_cost=sum(upgrade_cost)/n_run,upgrade_grant=sum(upgrade_grant)/n_run)
grants_cumulative <- grants_cumulative %>% group_by(grant_type) %>% mutate(upgrade_cost=cumsum(upgrade_cost)/800*housing_stock_oo/1e+6,upgrade_grant=cumsum(upgrade_grant)/800*housing_stock_oo/1e+6)
#
grants_cumulative <- grants_cumulative %>% pivot_longer(cols=c(upgrade_cost,upgrade_grant), values_to="Meuro",names_to="component")
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
#TWh
completions <- completions %>% ungroup() %>% mutate(space_heating_requirement_wem = cumsum(space_heating_requirement_wem)/1e+9,fec_wem=cumsum(fec_wem/1e+9),
                                                    space_heating_requirement_wam = cumsum(space_heating_requirement_wam)/1e+9,fec_wam=cumsum(fec_wam/1e+9)
                                                    )
new_housing_stock <- completions %>% mutate(wem=cumsum(wem),wam = cumsum(wam)) %>% select(-hli,-ber,-heat_pump_fraction,-floor_area,-efficiency_gas,-efficiency_hp)
#
fec_pre_2015 <- fec %>% group_by(year) %>% summarise(fec_pre_2015=mean(fec_prebound)/1e+9)
fec_new <- new_housing_stock %>% select(year,fec_wem) %>% rename("fec_new"=fec_wem)
fec_all <- fec_pre_2015 %>% inner_join(fec_new) %>% mutate(fec=fec_new+fec_pre_2015)
#
fec_all <- fec_all %>% select(-fec) %>% pivot_longer(cols=-year)

g <- fec_all %>% ggplot()+ geom_area(aes(year,value,fill=name))
g <- g + theme_minimal() + scale_fill_canva()
eed <- tibble(x=2023,xend=2030,y=20.84712,yend=20.84712*(1-0.125)) #2030 target 18.2 TWh
g <- g + geom_segment(data=eed,aes(x=x,y=y,xend=xend,yend=yend), linewidth=1,colour="grey50") + geom_point(data=eed,aes(xend,yend),size=2)
g <- g + geom_hline(yintercept = 18.24123, linetype="dotted") #+ scale_y_continuous(trans="sqrt")
g <- g + coord_cartesian(ylim=c(10,25))
#
export::graph2ppt(g,"~/Policy/CAMG/EED/Heat/fec_target_wam_closeup.ppt")

fec_all %>% filter(year %in% c(2030,2040))


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

