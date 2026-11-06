#' @title efficiency_cal
#' @description cumulative total number of b2 or better upgrades and heat pumps installed with SEAI grant assistance on the calibrations dates
#' Note he that B2s achieved through solar PV installations are removed from this data. Remarkably few B2 are achieved through fabric upgrades alone
#' @format A data frame with 3 rows and 3 variables:
#' \describe{
#'   \item{\code{date}}{double date}
#'   \item{\code{n_b2_obsv}}{double B2s}
#'   \item{\code{n_heat_pump_obsv}}{double heat pumps}
#'}
#' @details \url{https://www.seai.ie/grants/home-energy-grants/home-upgrades}
"efficiency_cal"
