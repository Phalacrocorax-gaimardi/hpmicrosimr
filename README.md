
<!-- README.md is generated from README.Rmd. Please edit that file -->

# hpmicrosimr

<!-- badges: start -->

<!-- badges: end -->

hpmicrosimr is an agent-based model framework describing home energy
efficiency and heating technology system upgrades by 901 owner-occupier
households from 2015.

This is an early development version of hpmicrosimr. So far the only
functionality is the financial returns (bill savings) and the
initialiser. The household investment decision involves a possible BER
upgrade from BER_old -\> BER_new and a possible technology shift from
tech_old -\> tech_new. hpmicrosimr calculates the financial return
tensor, including the effect of grants if applicable.

## Installation

You can install the development version of hpmicrosimr like so:

``` r
remotes::install_github("https://github.com/Phalacrocorax-gaimardi/hpmicrosimr")
```

## Example

A Munster household heats their detached 2003 house with a C2 BER score
with a oil boiler as their primary heating source. We would like to know
their financially optimal upgrade path before applying behavioural
correction factors such as risk aversion, hassle, present bias etc.

First load in the technical and cost parameters for 2026. Heating system
technology costs are assumed to be fixed (apart from labour).

``` r
library(hpmicrosimr)
params <- scenario_params(sD,2026)
tech_params <- tech_params_fun()
optimise_upgrade(ber_old=180,tech_old = "oil",house_type="detached",2003,region="Munster",floor_area=100,params)
#> # A tibble: 5 × 5
#>   tech_new    annualised_cost ber_optimal annualised_cost_old bill_savings
#>   <chr>                 <dbl>       <dbl>               <dbl>        <dbl>
#> 1 heat_pump             1540.        77.7               2756.        -44.1
#> 2 oil                   1615.        77.7               2756.        -41.4
#> 3 gas                   1802.        67.9               2756.        -34.6
#> 4 electricity           2369.        19.8               2756.        -14.1
#> 5 solid_fuel            1756.        67.9               2756.        -36.3
```

The optimum upgrade in this case is to a B1 with a switch to a Heat
Pump. However, replacing their oil boiler is a close second.
