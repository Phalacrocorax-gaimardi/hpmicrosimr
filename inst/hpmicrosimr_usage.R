#hpmicrosimr usage
library(hpmicrosimr)
sessionInfo()
library(tidyverse)

sD_cal <- readxl::read_xlsx("inst/extdata/scenario_parameters.xlsx",sheet="WEM")
sD_cal[sD_cal$parameter=="night_rate_usage_factor","value"] <- 0
sD_cal[sD_cal$parameter=="nu.","value"] <- 0.2


params <- scenario_params(sD_cal,2026)
params$night_rate_usage_factor

calABM(sD_cal,4,2,FALSE,0.15,0.0022,0.05,0.7,1,0.3)

calABM(sD_cal,4,2,FALSE,0.2,0,0.006,0.05,0.7,0.7,0.3)

########
test <- runABM(sD_cal,4,2050,F,2,F,F,F)

test2 <- test[[1]] %>% group_by(date,tech) %>% summarise(n=n()/4)
test2 <- test2 %>% ungroup() %>% arrange(date,factor(tech,levels = c("heat_pump","electricity","oil","gas","solid_fuel")))
test2 <- test2 %>% mutate(tech=factor(tech,levels=rev(c("heat_pump","electricity","oil","gas","solid_fuel"))))
housing_stock_oo <- 611877+535675
n_0 <- length(test[[1]]$serial %>% unique())
g <- test2 %>% ggplot(aes(date,n/n_0*housing_stock_oo,fill=tech))+geom_area()
g <- g + theme_minimal() + scale_fill_viridis_d()
export::graph2ppt(g,"~/Policy/CAMG/EED/Heat/test_uptake2.ppt")
