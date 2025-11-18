
<!-- README.md is generated from README.Rmd. Please edit that file -->

# hpmicrosimr

<!-- badges: start -->

<!-- badges: end -->

*hpmicrosimr* is an agent-based model simulation framework describing
home energy efficiency and heating technology system upgrades by
$`\approx 800`$ Irish owner-occupier households. Typically the model is
initialised in 2015 and is run to 2040 or 2050. Agent characteristics
are based on survey data collected in 2024.

This is the development version of *hpmicrosimr*. The main functionality
is (1) a detailed model of financial return on household energy
investments including choice of heating technology (2) an initialiser,
(3) an **updater** and (4) runABM. Agents live on a artificial social
network to allow peer effects in heat pump adoption to be described.

The heating systems included in the current version of *hpmicrosimr* are
oil, solid_fuel, gas, electric and air source heat_pumps. There are two
underlying space heating upgrade processes describe by *hpmicrosimr* at
each time step. The most common event is failure and replacement of the
current heating system.Failure probability is described by a Weibull
distribution with technology-specific factors. The current version of
*hpmicrosimr* assumes that the full heating requirement is supplied by
the primary heating source. Backup secondary and tertiary heat sources
do not play a role in the modelling. Agents choose between replacing the
current fuel source with the same tech or adopting a heat pump. Risk
aversion and “status quo bias” lowers the rate of adoption of heat pumps
even in cases where this offers a better financial return. Possibilities
such as oil $`\rightarrow`$ gas are excluded as this is not an option
for households not near the gas network.

A less common, but more significant, event from the point of view of
energy efficiency is the decision by a household to carry out a home
energy efficiency retrofit. The rate at which households take this
option is set by a parameter *p.* fit from calibration to historical
data. The household investment decision involves a choice of $`BER`$
upgrade from $`BER_{old} \rightarrow BER_{new}`$, as well as a potential
technology shift from $`tech_{old} \rightarrow tech_{new}`$. The
household may also choose to retain their current heating system. This
choice may be advantageous if the expected residual value of the
existing asset is high. A function **optimise_upgrade()** determines the
optimum upgrade choice, including the effect of all currently applicable
grants.

Applications of *hpmicrosimr* are to project energy efficiency outcomes
based on future policy scenarios (such as WEM/WAM), impacts on CO2
emissions, and associated cost-benefit analysis of generous incentive
schemes.

There are large behavioural preferences that influence energy efficiency
outcomes. There is strong evidence (Coyne & Denny) of “temperature
take-back” or rebound effects following space heating efficiency
upgrades. *hpmicrosimr*

A separate package *hpmicrocalibrater* is used to generate the model
weights and thresholds and are provided in a dataset *agents_init*
attached to *hpmicrosimr*.

## Installation

To install the latest development version of *hpmicrosimr*:

``` r
remotes::install_github("https://github.com/Phalacrocorax-gaimardi/hpmicrosimr")
```

## Financial Returns

Householders face a complex decision when considering home energy
upgrades. *hpmicrosimr* has a number of functions that determine the
financially optimum upgrade, taking grantsinto account. The financial
return must be sufficient to overcome any behavioural correction factors
such as risk aversion, hassle, present bias, likely rebound etc before
adoption occurs.

First load in the technical and cost scenario parameters for 2026
*params*. A set of technology specific cost parameters are contained in
*tech_parms* and are assumed to be fixed (apart from skilled labour
cost).

``` r
library(hpmicrosimr)
params <- scenario_params(sD,2026)
tech_params <- tech_params_fun()
#optimise_upgrade(ber_old=180,tech_old = "oil",house_type="detached",2003,region="Munster",floor_area=100,params, is_fuel_allowance=FALSE)
```

The optimum upgrade in this case is to a B1 with a switch to a Heat
Pump. However, replacing their oil boiler is a close second. Note that
this calculation assumes no change in the heating comfort level demanded
by households. This assuming is relaxed during the calibration stage
when behavioural parameters are introduced.

### efficiency retrofit cost model

The default cost model for energy efficiency improvements used by
*hpmicrosimr* does not consider specific measures such as lift
insulation, new windows etc. Instead it is based on a marginal cost
model $`\frac{k_0}{BER^{\alpha}}`$. This reflects the steep increase in
that upgrade costs required to reach the most efficient ratings A2 etc.
The resulting efficiency upgrade matrix is shown below.

``` r
library(hpmicrosimr)
params <- scenario_params(sD,2025)
hpmicrosimr::gen_upgrade_cost_matrix(house_type="semi_detached","Dublin",120,include_grant = FALSE,params,model="marginal")
#> # A tibble: 16 × 17
#>    ber_old      A     A1     A2     A3     B1     B2     B3     C1     C2     C3
#>    <chr>    <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>
#>  1 A       3.18e3 0      0      0      0          0      0      0      0      0 
#>  2 A1      2.20e4 2.91e3 0      0      0          0      0      0      0      0 
#>  3 A2      4.23e4 2.02e4 3.30e3 0      0          0      0      0      0      0 
#>  4 A3      5.97e4 3.76e4 1.74e4 2.37e3 0          0      0      0      0      0 
#>  5 B1      7.29e4 5.09e4 3.07e4 1.33e4 1.90e3     0      0      0      0      0 
#>  6 B2      8.39e4 6.18e4 4.16e4 2.42e4 1.10e4  1616.     0      0      0      0 
#>  7 B3      9.33e4 7.13e4 5.11e4 3.37e4 2.04e4  9450.  1417.     0      0      0 
#>  8 C1      1.02e5 7.97e4 5.94e4 4.20e4 2.88e4 17828.  8378.  1269.     0      0 
#>  9 C2      1.09e5 8.72e4 6.70e4 4.96e4 3.63e4 25394. 15944.  7566.  1155.     0 
#> 10 C3      1.16e5 9.42e4 7.39e4 5.65e4 4.33e4 32321. 22870. 14493.  6927.  1064.
#> 11 D1      1.24e5 1.02e5 8.15e4 6.41e4 5.09e4 39938. 30488. 22110. 14544.  7618.
#> 12 D2      1.33e5 1.10e5 9.02e4 7.29e4 5.96e4 48644. 39194. 30816. 23250. 16323.
#> 13 E1      1.41e5 1.19e5 9.87e4 8.13e4 6.81e4 57126. 47675. 39298. 31732. 24805.
#> 14 E2      1.49e5 1.27e5 1.07e5 8.91e4 7.59e4 64930. 55480. 47102. 39536. 32609.
#> 15 F       1.59e5 1.37e5 1.16e5 9.89e4 8.57e4 74735. 65284. 56907. 49341. 42414.
#> 16 G       1.76e5 1.54e5 1.34e5 1.16e5 1.03e5 91955. 82505. 74127. 66561. 59635.
#> # ℹ 6 more variables: D1 <dbl>, D2 <dbl>, E1 <dbl>, E2 <dbl>, F <dbl>, G <dbl>
#optimise_upgrade(ber_old=180,tech_old = "oil",house_type="detached",2003,region="Munster",floor_area=100,params, is_fuel_allowance=FALSE)
retrofit_cost_model_marginal(200,100,"semi_detached","Dublin",120,scenario_params(sD,2026))
#> [1] 34535.57
```

### Heating system capital and operating costs

Capital costs for each technology depend on the time of installation,
the system capacity (kW), whether the system replaces an earlier system
of the same fuel type, etc. For example, the influence of grants is
shown in the example below.

``` r
library(hpmicrosimr)
params <- scenario_params(sD,2026)
tech_params <- tech_params_fun()
#18kW output heat pump cost before grant
heating_system_capital_cost("heat_pump",kW=18,installation_type="new",house_type="detached",construction_year=2000,grant_type="None",params)
#> [1] 15620
#effect of grants
heating_system_capital_cost("heat_pump",18,"new","detached",2000,"BetterEnergyHomes",params)
#> [1] 9120
```

Annual operating costs are sensitive to BER which determines the heat
requirement of the property. These depend on the current time
params\$yeartime but also the installation time of the system. This is
because efficiency has changed significantly in the past e.g. with the
introduction of condensing boilers.

``` r
library(hpmicrosimr)
params <- scenario_params(sD,2026)
tech_params <- tech_params_fun()
#C3 rating
heating_system_operating_cost(tech="oil",installation_time=2003,ber=210,floor_area=100,params,include_rebound=FALSE)
#> [1] 3532.667
#B1 rating
heating_system_operating_cost(tech="oil",installation_time=2003,ber=80,floor_area=100,params,include_rebound=FALSE)
#> [1] 976
```

The notional operating costs calculated above assume standard heating
season conditions and a fully heated property (*include_rebound=FALSE*).
Operating costs calculated when households trade comfort for cost should
be calculated using *include_rebound=TRUE*. Assuming a large rebound of
50% ( *params\$r.*):

``` r
library(hpmicrosimr)
params <- scenario_params(sD,2026)
tech_params <- tech_params_fun()
#C3 rating
heating_system_operating_cost(tech="oil",installation_time=2003,ber=210,floor_area=100,params,include_rebound=TRUE)
#> [1] 1988.833
#B2 rating
heating_system_operating_cost(tech="oil",installation_time=2003,ber=120,floor_area=100,params,include_rebound=TRUE)
#> [1] 1103.833
```

### Effective Annual Costs

A key concept for the modelling is Effective Annual Cost (EAC). The EAC
represent the annual “bill”. This includes the actual heating bill as
well as the annualised capital cost of heating technology and any
efficiency upgrade undertaken. For example, a heat pump is adopted if
the EAC gain relative to a competing technology such as gas is
sufficient. This is a complex calculation because the optimal BER
upgrade for gas and heat pumps may differ. For real technologies with
uncertain lifetimes, EAC declines as the lifetime of the system is
approached. *hpmicrosimr* calculates EAC and expected system lifetimes
using technology-specific Weibull hazard functions.

Effective annual costs (EACs) of heating technologies have changed over
time due higher installation (labour) costs, efficiency gains, fuel cost
changes ad grants. To illustrate this, early year EACs for heat pump,
gas and oil systems installed during 2010-2025 is shown below, using the
*hpmicrosimr::annualised_heating_system_cost()*. The property has BER of
175kWh/m2/year in this example. The impact of the introduction of
capital grants in 2018 for heat pumps is obvious. This calculation
suggests that heat pumps are the lowest cost for this C1/C2 rated
household but this assumes that a substantial use of night-rated
electricity. The rebound effect is not included. Rebound lowers the
operating cost and therefore makes heat pumps appear less attractive.

``` r
library(hpmicrosimr)
library(tidyverse)
#> ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
#> ✔ dplyr     1.1.4     ✔ readr     2.1.5
#> ✔ forcats   1.0.0     ✔ stringr   1.5.1
#> ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
#> ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
#> ✔ purrr     1.0.2     
#> ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
#> ✖ dplyr::filter() masks stats::filter()
#> ✖ dplyr::lag()    masks stats::lag()
#> ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
eac <- tibble()
for(year in 2010:2025){
params <- scenario_params(sD,year)
ber <- 175
incl_rebound <- FALSE
hp <- annualised_heating_system_cost("heat_pump", params$yeartime,"new",ber,100,"detached",2000,"None",params,include_rebound=incl_rebound)
hp_grant <- annualised_heating_system_cost("heat_pump", params$yeartime,"new",ber,100,"detached",2000,"BetterEnergyHomes",params,include_rebound=incl_rebound)
oil <- annualised_heating_system_cost("oil", params$yeartime,"new",ber,100,"detached",2000,"None",params,include_rebound=incl_rebound)
gas <- annualised_heating_system_cost("gas", params$yeartime,"new",ber,100,"detached",2000,"None",params,include_rebound=incl_rebound)
electric <- annualised_heating_system_cost("electricity", params$yeartime,"new",ber,100,"detached",2000,"None",params,include_rebound=incl_rebound)
eac <- eac %>% bind_rows(tibble(year=year,hp_nogrant=hp,hp_grant=hp_grant,oil=oil,gas=gas))
}
eac %>% pivot_longer(cols=-year) %>% ggplot(aes(year,value,colour=name))+geom_line()
```

<img src="man/figures/README-historical eacs-1.png" width="100%" />

### optimum upgrade

Familiarity with three high-level functions needed to run *hpmicrosimr*:
the initialiser *initial_agents()*, the updater *update_agents()* and
*run_abm()*.

## Initialiser

The function *initialise_agents()* generates an initial state of the
population of agents at a specific time.

This function does quite a bit of work behind the scenes. It is based on
2024 survey data, projected backwards to, say, 2015. It imputes missing
values of BER, construction year and household income, for example. It
also imputes the floor area of the property. If the initialisation time
of the ABM is set to 2015, then only houses constructed before 2015 are
included. This gives a full decade for model calibration, but at the
cost of a reduced sample size. Note that houses built after 2015 are not
eligible for energy efficiency grants, apart from solar PV.

If the installation year of the current heating system stated in the
survey is later then 2015 then an installation date before 2015 is
inferred for the earlier system. It is assumed that the heating
technology used by the household in 2015 is the same as the technology
used in 2024. The only exception is for heat pumps where it is assumed
that heat pumps adopted after 2015 replaced an earlier gas or oil
system. *initialise_agents()* also does a number of other recodings of
the survey variables. The complete set of survey questions and responses
are provided in the datasets *hpmicrosimr::hp_questions* and
*hpmicrosimr::hp_qanda*.

The initial state contains all data needed to evaluate EACs for each
agent for all possible technology choices. It uses the survey input data
for owner-occupiers *hp_survey_oo*. *initialise_agents()* imputes
missing values of BER ratings (based on modelling of the SEAI BER
dataset), household income etc. It also imputes the total floor area of
the property statistically based on number of bedrooms, region,
area_type etc. A new initial state is generated for each ABM run
(randomisation). This ensures that the results do not depend on a
particular values of statistically imputed variables.

## Example: initial state rebound effect

The example below illustrates the use of *initialise_agents*. The
initial annual heating requirements for each household is calculated.
The figure shows the resulting distribution of a kWh/year values with
and without rebound of 30%.

``` r
library(hpmicrosimr)
library(ggplot2)
params <- scenario_params(sD,2015)
agents_in <- initialise_agents(sD,2015, cal_run=sample(1:100,1))
#> Joining with `by = join_by(q6)`
#> Joining with `by = join_by(qc2)`
#> Joining with `by = join_by(q1)`
#> Joining with `by = join_by(serial)`
#> Joining with `by = join_by(qh)`
heat <- agents_in %>% dplyr::rowwise() %>% dplyr::mutate(q_norebound=heating_requirement(ber,floor_area,rebound=1,params))
heat <- heat %>% dplyr::rowwise() %>% dplyr::mutate(q_rebound=heating_requirement(ber,floor_area,rebound=0.7,params))
heat <- heat %>% dplyr::select(serial,q_norebound,q_rebound) %>% tidyr::pivot_longer(cols=-serial)
heat %>% dplyr::filter(value < 1e+5) %>% ggplot(aes(value,fill=name))+geom_density(alpha=0.5) #+ facet_wrap(.~name)
```

<img src="man/figures/README-initialiser}-1.png" width="100%" />

In the above example, 792 households is equivalent to initial
theoretical heating requirement of 19.5 TWh with 30% rebound for Ireland
as a whole. Without rebound it is equivalent to $`\approx`$ 26 TWh.

The heating requirements or Final Energy Consumption (FEC) will change
as the ABM runs and agents make upgrade choices. The FEC is not very
sensitive to the heating technology choices by the agents. On the other
hand, CO2 emissions are sensitive to technology choices.

## Example: Updater

*update_agents* is the core function that advances the system by one
time step

``` r
library(hpmicrosimr)
help("update_agents")
```
