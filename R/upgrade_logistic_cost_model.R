#' @title upgrade_logistic_cost_model
#' @description fabric upgrade logistic cost model parameters
#' @format A data frame with 7 rows and 6 variables:
#' \describe{
#'   \item{\code{dwelling_type}}{character detached etc}
#'   \item{\code{no_storeys}}{double storeys}
#'   \item{\code{c_min}}{double marginal cost in the inefficient limit}
#'   \item{\code{c_max}}{double maginal cost in the efficient limit}
#'   \item{\code{h_0}}{double cross-over offset}
#'   \item{\code{k}}{double crossover scale. Higher k is more rapid increase in costs}
#'}
#' @details DETAILS
"upgrade_logistic_cost_model"
