% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/technicals.R
\name{gen_ber_rating}
\alias{gen_ber_rating}
\title{gen_ber_rating}
\usage{
gen_ber_rating(ber_score, g_rate = 1/100)
}
\arguments{
\item{ber_score}{character}

\item{g_rate}{exponential rate, defauly 1/100}
}
\value{
double
}
\description{
Stochastically generate a ber rating (kWh/m2/year) from a ber score. G ratings are exponentially distributed from 450kWh/m2/year with
a default decay rate 0.01.
}
\examples{
gen_ber_rating("B3")
}
