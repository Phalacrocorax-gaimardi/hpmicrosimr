
<!-- README.md is generated from README.Rmd. Please edit that file -->

# hpmicrosimr

<!-- badges: start -->

<!-- badges: end -->

*hpmicrosimr* is an agent-based model simulation framework describing
home energy efficiency and heating technology system upgrades by 901
owner-occupier households. The model is initialised in, say, 2015 and
can run to 2050.

This is an early development version of *hpmicrosimr*. The main
functionality is the household financial return (bill savings). There is
also an initialiser. The household investment decision involves a
possible BER upgrade from BER_old -\> BER_new and a possible technology
shift from tech_old -\> tech_new. *hpmicrosimr* calculates the full
financial return tensor, and determines the optimum system, including
the effect of grants applicable.A separate package *hpmicrocalibrater*
is used to generate the model weights and thresholds and are provided in
a dataset *agents_init* attached to *hpmicrosimr*.

## Installation

The latest development version of *hpmicrosimr* can be installed as
follows:

``` r
remotes::install_github("https://github.com/Phalacrocorax-gaimardi/hpmicrosimr")
```

## Example: Financial Return Tensor

Householders face a complex decision when considering home energy
upgrades. For example, a detached house in Munster with 100m2 floor area
is heated using an oil boiler in 2026. The house was built in 2003 and
has a C2 BER score. The current version of *hpmicrosimr* ignores any
secondary and tertiary heat sources that may be present or might be
adopted by a household. With this assumption, we would like to know the
financially optimal upgrade path. If the financial return is sufficient,
this can overcome behavioural correction factors such as risk aversion,
hassle, present bias etc.

First load in the technical and cost scenario parameters for 2026
*params*. A set of technology specific cost parameters are contained in
*tech_parms* and are assumed to be fixed (apart from skilled labour
cost). The default cost matrix for energy efficiency improvements is
based on a marginal cost model $`k_0 BER^{-\alpha}`$. This captures the
steep increase in that upgrade costs required to reach the lowest heat
loss ratings A1, A2 etc.

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

### Financial return examples

Familiarity with three high-level functions needed to run *hpmicrosimr*:
the initialiser *initial_agents()*, the updater *update_agents()* and
*run_abm()*.

## Example: initialiser

The function *initialise_agents()* generates the initial state of the
population of agents at a specific time. It based on 2024 survey data,
projected backwards to, say, 2015.

This function does quite a bit of work behind the scenes.It imputes
missing values of BER, construction year and household income, for
example. It also imputes the floor area of the property. If the
initialisation time of the ABM is set to 2015, then only houses
constructed before 2015 are included. If the installation year of the
current heating system stated in the survey is later then 2015 then an
installation date before 2015 is inferred for the earlier system. It is
assumed that the heating technology used by the household in 2015 is the
same as the technology used in 2024. The only exception is for heat
pumps where it is assumed that heat pumps adopted after 2015 replaced an
earlier gas or oil system. *initialise_agents()* also does a number of
other recodings of the survey variables descroved in *hp_questions* and
*hp_qanda*.

The initial state contains all data needed to evaluate the annual bill
savings for each agent for all possible technology choices. It uses the
survey input data for owner-occupiers *hp_survey_oo*. A new initial
state is generated for each ABM run (randomisation). This ensures that
the results do not depend on a particular set of imputed variables.

``` r
library(hpmicrosimr)
initialise_agents(sD,2015, cal_run=sample(1:100,1))
#> Joining with `by = join_by(q6)`
#> Joining with `by = join_by(qc2)`
#> Joining with `by = join_by(q1)`
#> Joining with `by = join_by(serial)`
#> Joining with `by = join_by(qh)`
#> # A tibble: 789 × 21
#>    serial w_q52 w_q13 w_theta region    house_type    q2 construction_year   ber
#>    <chr>  <dbl> <dbl>   <dbl> <chr>     <chr>      <dbl>             <dbl> <dbl>
#>  1 71     0.857 0.987  1.12   Ulster/C… detached       2              2004 147. 
#>  2 72     1.16  1.07   0.910  Dublin    semi_deta…     2              1997 108. 
#>  3 74     1.29  0.902  0.725  Dublin    terraced       2              1960 188. 
#>  4 75     1.18  1.25  -0.203  Dublin    apartment      2              2007  28.4
#>  5 77     1.28  1.41   0.0941 Munster   detached       2              1998 129. 
#>  6 79     1.19  1.29   0.746  Munster   detached       2              1998 326. 
#>  7 81     1.08  1.31   1.21   Munster   detached       1              2004 136. 
#>  8 82     0.976 0.931  1.06   Ulster/C… detached       1              2006  75.1
#>  9 84     1.03  1.40   0.525  Rest of … semi_deta…     2              2000 183. 
#> 10 87     0     1.15   1.87   Rest of … detached       2              1989 105. 
#> # ℹ 779 more rows
#> # ℹ 12 more variables: floor_area <dbl>, primary_heat <chr>,
#> #   heating_install_time <dbl>, secondary_heat1 <chr>, secondary_heat2 <chr>,
#> #   income <dbl>, fuel_allowance <lgl>, kW <dbl>, q52 <dbl>, annual_cost <dbl>,
#> #   heat_pump_grant <dbl>, upgrade_grant <dbl>
```

## Example: Updater

*update_agents* is the core function that advances the system by one
time step

``` r
library(hpmicrosimr)
help("update_agents")
#> starting httpd help server ... done
```
