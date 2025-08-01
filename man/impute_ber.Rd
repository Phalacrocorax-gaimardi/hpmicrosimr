% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/technicals.R
\name{impute_ber}
\alias{impute_ber}
\title{impute_ber}
\usage{
impute_ber(house_type, q2_0, region, q5_0, q3_0)
}
\arguments{
\item{house_type}{property type}

\item{q2_0}{number of storeys}

\item{region}{region}

\item{q5_0}{year property built}

\item{q3_0}{number of bedrooms}
}
\value{
double, ber rating
}
\description{
generate a missing Ber rating (double) based on household characteristics
}
\examples{
impute_ber("semi-detached",2,"Dublin",4,4)
}
