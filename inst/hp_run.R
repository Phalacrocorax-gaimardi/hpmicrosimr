###########################
# WEM
###########################

library(hpmicrosimr)
library(tidyverse)

sD_wem <- readxl::read_xlsx("/home/people/jwheatley/hpretrofit/runs/scenario_parameters.xlsx",sheet="WEM") 

wem <- runABM(sD_wem,32,2040)
writeRDS(wem,"/home/people/jwheatley/hpretrofit/runs/wem.RData")