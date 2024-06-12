#' Toy Stock Data
#'
#' A simulated dataset for demonstrating plotting functions in the NAFOdown R package. The dataset contains simulated fish stock dynamics over a 40-year period.
#'
#' @format A data frame with 40 rows and 13 variables:
#' \describe{
#'   \item{year}{Year of the simulation, ranging from 1981 to 2020.}
#'   \item{biomass}{Simulated biomass of the fish stock.}
#'   \item{rec_index1}{Simulated recruitment index 1.}
#'   \item{rec_index2}{Simulated recruitment index 2.}
#'   \item{rec_index3}{Simulated recruitment index 3.}
#'   \item{TAC}{Total allowable catch (TAC) for the fish stock.}
#'   \item{catch}{Simulated catch of the fish stock.}
#'   \item{Fmsy}{Fishing mortality rate at maximum sustainable yield.}
#'   \item{Bmsy}{Biomass at maximum sustainable yield.}
#'   \item{relB}{Relative biomass compared to Bmsy.}
#'   \item{relB_lwr}{Lower bound of relative biomass confidence interval.}
#'   \item{relB_upr}{Upper bound of relative biomass confidence interval.}
#'   \item{relF}{Relative fishing mortality rate compared to Fmsy.}
#'   \item{relF_lwr}{Lower bound of relative fishing mortality rate confidence interval.}
#'   \item{relF_upr}{Upper bound of relative fishing mortality rate confidence interval.}
#' }
#'
#' @details
#' The data were generated using a simple surplus production model.
#'
"toy_stock"
