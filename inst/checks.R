#checks


hp_data_in <- hp_survey_oo %>% dplyr::select(serial,qb,actualage,qh,qi,q1,qc2,q2,q3,q5,q6,q7,q11,q13n,q14n,q15n,q16n,q17n,q18n,q19n,q20n,q21n,q28,q121,q122,q123,q124,q128)

hp_surv <- recode_survey(hp_data_in)
#convert all falts and other to singel stoey (ambiguity in question)
hp_surv <- hp_surv %>% mutate(q2 = replace(q2,q1 %in% c("Other","Flat or apartment"),1))

#annual bills from survey (q13)
#hp_surv <- hp_surv %>% inner_join(heating_bill_values %>% r)
gas_prices <- readr::read_csv("C:/Users/Joe/pkgs/hpmicrosimr/inst/extdata/gas_prices.csv") %>% dplyr::mutate(fuel="gas")
electricity_prices <- readr::read_csv("C:/Users/Joe/pkgs/hpmicrosimr/inst/extdata/electricity_prices.csv") %>% dplyr::mutate(fuel="electricity")
oil_prices <- readr::read_csv("C:/Users/Joe/pkgs/hpmicrosimr/inst/extdata/oil_prices.csv") %>% dplyr::mutate(fuel="oil")
solid_fuel_prices <- readr::read_csv("C:/Users/Joe/pkgs/hpmicrosimr/inst/extdata/wood_prices.csv") %>% dplyr::mutate(fuel="solid_fuel")

energy_prices <- gas_prices %>% dplyr::bind_rows(oil_prices,solid_fuel_prices,electricity_prices)
#
hp_surv <- hp_surv %>% inner_join(energy_prices %>% filter(year==2024) %>% rename("q11"=fuel))
hp_surv <- hp_surv %>% mutate(implied_bill = price*heating_requirment/100)

#hp_surv <- hp_surv %>% mutate(annual_heating_hours = annual_heating_hours(q20,q21))
hp_surv <- hp_surv %>% mutate(annual_heating_hours = q20n*q21n*30)
#
hp_surv <- hp_surv %>% mutate(primary_heating = q11, secondary_heat = case_when(q121==1~"oil",q122==1~"gas",q123==3~"electricity",q124==1~"solid fuels",q128==1~"none"))

hp_surv <- hp_surv %>% select(-q11,-q121,-q122,-q123,-q124,-year)

###########################
# check consistency between annual heating bill and primary/secondary heating bills
############################

multi_fuel_cost_estimate <- function(primary_fuel,secondary_fuel,q14n,q15n,q16n,q17n,q18n,q19n){

  case_when(primary_fuel=="gas"& secondary_fuel=="none"~6*(q17n+q18n)/2,
                    primary_fuel=="gas"& secondary_fuel=="solid_fuels"~6*(q17n+q18n)/2+ q19n,
                    primary_fuel=="gas" & secondary_fuel=="oil"~6*(q17n+q18n)/2 + 0.99*q16n,
                    primary_fuel=="electricity"& secondary_fuel=="none"~6*(q14n+q15n)/2,
                    primary_fuel=="electricity"& secondary_fuel=="gas"~6*(q14n+q15n)/2+6*(q17n+q18n)/2,
                    primary_fuel=="electricity"& secondary_fuel=="oil"~6*(q14n+q15n)/2 + 0.99*q16n,
                    primary_fuel=="electricity"& secondary_fuel=="solid fuels"~6*(q14n+q15n)/2 + q19n,
                    primary_fuel=="oil"& secondary_fuel=="gas"~6*(q17n+q18n)/2 + 0.99*q16n,
                    primary_fuel=="oil"& secondary_fuel=="none"~0.99*q16n,
                    primary_fuel=="oil"& secondary_fuel=="solid fuels"~q19n + 0.99*q16n,
                    primary_fuel=="solid fuels"& secondary_fuel=="gas"~q19n + 6*(q17n+q18n)/2,
                    primary_fuel=="solid fuels"& secondary_fuel=="none"~q19n,
                    primary_fuel=="solid fuels"& secondary_fuel=="oil"~q19n + 0.99*q16n)
}

hp_surv <- hp_surv %>% mutate(cost_estimate = multi_fuel_cost_estimate(primary_heating,secondary_heat,q14n,q15n,q16n,q17n,q18n,q19n))

hp_surv %>% filter(secondary_heat=="none", cost_estimate < 5000, implied_bill < 10000) %>% select(q13n,cost_estimate,implied_bill) %>% drop_na() %>% ggplot(aes(cost_estimate, implied_bill))+geom_point()
#
lm(implied_bill ~ cost_estimate,hp_surv %>% filter(cost_estimate < 5000, implied_bill < 10000)) %>% summary()

hp_surv %>% filter(secondary_heat=="none") %>% ungroup() %>% slice_max(implied_bill)


hp_surv %>% ungroup() %>% slice_max(implied_bill)

#is annual heating hours correlated with annual bill?
hp_surv %>% ggplot(aes(annual_heating_hours*ground_floor_area*q2,heating_bill, colour=q11))+geom_point() + facet_wrap(.~q11)
lm(heating_bill~I(annual_heating_hours*ground_floor_area*q2), hp_surv %>% filter(primary_heating=="gas", secondary_heat=="none")) %>% summary()
lm(implied_bill~I(annual_heating_hours*ground_floor_area*q2), test) %>% summary()


test <- test %>% inner_join(energy_prices %>% filter(year==2024) %>% rename("q11"=fuel)) %>% mutate(annual_bill=price*heating_requirment/100) %>% inner_join(heating_bill_values)
test <- test %>% inner_join(energy_prices %>% filter(year==2024) %>% rename("q11"=fuel)) %>% mutate(annual_bill=price*heating_requirment/100) %>% inner_join(heating_bill_values)

test0 <- hp_survey_oo %>% mutate(primary_heating = recode_fuels(q11), secondary_heat = case_when(q121==1~"oil",q122==1~"gas",q123==3~"electricity",q124==1~"solid fuels",q128==1~"none"))

test %>% ggplot(aes(heating_bill,annual_bill,colour=q11)) + geom_point() + facet_wrap(.~q11)


#primary and secondary heating systems
test0 <- hp_survey_oo %>% mutate(primary_heating = recode_fuels(q11), secondary_heating = case_when(q121==1~"oil",q122==1~"gas",q123==3~"electricity",q124==1~"solid fuels",q128==1~"none"))

#rebound?
#do heating bills correlate with BER
g1 <- hp_surv %>% filter(!is.na(q6)) %>% group_by(BER=substr(q6,1,1)) %>% mutate(BER=replace(BER, BER %in% c("E","F","G"), "E-F-G"))  %>% ggplot(aes(BER,q13n, fill=BER))+geom_boxplot(varwidth = TRUE) + theme_minimal() + scale_y_continuous(limits=c(0,10000)) + scale_fill_viridis_d() + theme(legend.position="none")
g2 <- hp_surv %>% filter(!is.na(q6)) %>% group_by(BER=substr(q6,1,1)) %>% mutate(BER=replace(BER, BER %in% c("E","F","G"), "E-F-G"))  %>% ggplot(aes(BER,cost_estimate, fill=BER))+geom_boxplot(varwidth = TRUE) + theme_minimal() + scale_y_continuous(limits=c(0,10000)) + scale_fill_viridis_d()
#with impted heating requirement
g3 <- hp_surv %>% filter(!is.na(q6)) %>% group_by(BER=substr(q6,1,1)) %>% mutate(BER=replace(BER, BER %in% c("E","F","G"), "E-F-G")) %>% ggplot(aes(BER,implied_bill, fill=BER))+geom_boxplot(varwidth = TRUE) + theme_minimal() + scale_y_continuous(limits=c(0,10000)) + scale_fill_viridis_d() + theme(legend.position="none")
library(patchwork)
g1/g2/g3
#more detail on A, B, C
g1 <- hp_surv %>% filter(!is.na(q6), nchar(q6)==2, str_detect(q6,"A|B|C")) %>% group_by(q6)  %>% ggplot(aes(q6,q13n, fill=q6))+geom_boxplot(varwidth = TRUE) + theme_minimal() + scale_y_continuous(limits=c(0,10000)) + scale_fill_viridis_d() + theme(legend.position="none")
g2 <- hp_surv %>% filter(!is.na(q6), nchar(q6)==2, str_detect(q6,"A|B|C")) %>% group_by(q6)  %>% ggplot(aes(q6,cost_estimate, fill=q6))+geom_boxplot(varwidth = TRUE) + theme_minimal() + scale_y_continuous(limits=c(0,10000)) + scale_fill_viridis_d() + theme(legend.position="none")
#with impted heating requirement
g3 <- hp_surv %>% filter(!is.na(q6), nchar(q6)==2, str_detect(q6,"A|B|C")) %>% group_by(q6)  %>% ggplot(aes(q6,implied_bill, fill=q6))+geom_boxplot(varwidth = TRUE) + theme_minimal() + scale_y_continuous(limits=c(0,10000)) + scale_fill_viridis_d() + theme(legend.position="none")
g1/g2/g3
#
warmth <- hp_surv %>% filter(!is.na(q6)) %>% group_by(BER=substr(q6,1,1)) %>% mutate(BER=replace(BER, BER %in% c("E","F","G"), "E-F-G")) %>% summarise(warmth=mean(q28),n=n())
warmth <- warmth  %>% group_by(BER=substr(q6,1,1)) %>% summarise(warmth=mean(q28),n=n())
warmth %>% ggplot(aes(BER,warmth, fill=BER))+geom_col() + theme_minimal()  + theme(legend.position="none")

ggplot(aes(substr(q6,1,1),q28, fill=substr(q6,1,1)))+geom_point() + theme_minimal() #+ scale_y_continuous(limits=c(0,10000))
#with impted heating requirement



#with imputed bers
hp_surv %>% ggplot(aes(ber,implied_bill))+geom_point()
hp_surv %>% ggplot(aes(ber,q13n))+geom_point()




