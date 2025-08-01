#' @title ber_model_parameters
#' @description parameters for to impute missing BER values, depending on region (qc2), house type (q1),
#' storeys (q2), bedrooms(q3) and construction year (q5). Used in gen_roofsection_area().
#' @format A data frame with 1509 rows and 7 variables:
#' \describe{
#'   \item{\code{q1}}{character house type}
#'   \item{\code{q2}}{double number of stories}
#'   \item{\code{qc2}}{character region}
#'   \item{\code{q5}}{double construction year}
#'   \item{\code{q3}}{double bedrooms}
#'   \item{\code{mu}}{double mean}
#'   \item{\code{sigma}}{double standard deviation}
#'}
#' @details generated from Census 2022 (CSO F2063) and SEAI BER datasets
"ber_model_parameters"
