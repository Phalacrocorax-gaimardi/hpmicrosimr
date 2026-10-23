#' @title upgrade_logistic_cost_model
#' @description Parameters of the building fabric or envelope upgrade cost model. hpmicrosimr uses a logistic cost model with low marginal cost for inefficient buildings and
#' and high cost for efficient buildings. The parameters are defined through the marginal cost equation:
#'
#' \deqn{\frac{d C}{d HLI} = \frac{c_{min}}{1+e^{-\left(HLI-h_0\right)/k}} + \frac{c_{max}}{1+e^{\left(HLI-h_0\right)/k}}}
#'
#' where  is the cost and HLI is the Heat Loss Index.
#'
#' @format A data frame with 7 rows and 6 variables:
#' \describe{
#'   \item{\code{dwelling_type}}{character SEAI house type - detached etc}
#'   \item{\code{no_storeys}}{double number of storeys, 1, 2+}
#'   \item{\code{c_min}}{double marginal cost in the inefficient limit}
#'   \item{\code{c_max}}{double maginal cost in the efficient limit}
#'   \item{\code{h_0}}{double HLI cross-over offset value (typically 2-3)}
#'   \item{\code{k}}{double crossover scale factor width \eqn{\Delta HLI}. Small k is more rapid increase in costs in the vicinity of \eqn{h_0}}
#'}
#' @details Estimated from multiple sources. See detailed workings in /inst/scratch.R
"upgrade_logistic_cost_model"
