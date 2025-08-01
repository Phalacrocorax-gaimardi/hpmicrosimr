% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/technicals.R
\name{impute_hh_income}
\alias{impute_hh_income}
\title{impute_hh_income}
\usage{
impute_hh_income(hp_data_in)
}
\arguments{
\item{hp_data_in}{usually hp_survey_oo}
}
\value{
input dataframe with imputed qh values
}
\description{
imputes missing household income (qh) using data on car ownership from the household survey.
Uses random forest
}
\examples{
impute_hh_income(hp_survey_oo)
}
