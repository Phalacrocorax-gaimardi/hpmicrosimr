% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/finance.R
\name{heating_system_capital_cost}
\alias{heating_system_capital_cost}
\title{heating_system_capital_cost}
\usage{
heating_system_capital_cost(
  tech,
  kW,
  house_type,
  construction_year,
  installation = "swap",
  params,
  include_vat = TRUE,
  include_grant = FALSE
)
}
\arguments{
\item{tech}{primary heating technology}

\item{kW}{system capacity}

\item{house_type}{seai house type}

\item{construction_year}{year of construction, integer}

\item{installation}{"new" or "swap"}

\item{params}{current cost parameters output from scenario_params}

\item{include_vat}{TRUE/FALSE}

\item{include_grant}{TRUE/FALSE}
}
\value{
euro cost
}
\description{
cost of a new heating system. Includes VAT using the "two-thirds" rule.
}
\details{
The current version of this function assumed that ancilliary costs are halved if the same technology is being replaced.

The heat pump grant is included where eligible by default.
}
