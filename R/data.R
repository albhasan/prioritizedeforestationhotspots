#' Deforestation data for the Brazilian Amazon
#'
#' A dataset containing data regarding deforestation in the Brazilian Amazon.
#' These data have been aggregated into a regular grid made of cell of of 25
#' square kilometres.
#'
#' @format A data frame with 27132 rows and 14 variables:
#' \describe{
#'    \item{id}{Cell identifier}
#'    \item{area_PA}{protected areas or indigenous lands in the grid (square kilometres)}
#'    \item{dist_hidro}{distance to the closest waterway (kilometres)}
#'    \item{dist_road}{distance to the closest highway (kilometres)}
#'    \item{dist_road_hidro}{distance to the closest highway or waterway (kilometres)}
#'    \item{ref_year}{reference year}
#'    \item{def}{deforestation in the reference year (square kilometres)}
#'    \item{def_1_ly}{deforestation in the year before the reference year}
#'    \item{def_2_ly}{deforestation in the two years before the reference year}
#'    \item{def_4_ly}{deforestation in the four years before the reference year}
#'    \item{dist_1_percent_ly}{distance to the closest grid centroid with more than 1% deforestation in year before the reference year (kilometres)}
#'    \item{dist_2_percent_ly}{distance to the closest grid centroid with more than 2% deforestation in year before the reference year (kilometres)}
#'    \item{active_fires_ly}{Number of active fires in the year before the reference year}
#' }
"deforestation_data"



#' Deforestation grid for the Brazilian Amazon
#'
#' A dataset containing spatial data regarding the Brazilian Amazon. These data
#' represents a regular grid where each cell has an approximated area of 25*25
#' kilometres.
#'
#' The above ground biomass for 2018 was estimated from ESA Biomass Climate
#' Change Initiative (Biomass_cci) v3 as the mean of the pixels falling in each
#' cell after masking it using the areas identified as forest for 2021 by
#' PRODES.
#'
#' @format A data frame with 6783 rows and 2 variables:
#' \describe{
#'    \item{id}{Cell identifier}
#'    \item{geometry}{Cell geometry}
#'    \item{area_km2}{Area of each cell in square kilometers}
#'    \item{agb_2018_forest_2021}{Biomass in Megagrams per hectare for 2018 after using the 2021 forest mask}
#' }
"deforestation_grid"

