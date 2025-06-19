#' Income in Berlin, Germany
#' @description
#' A dataframe containing synthetic data on household income in Berlin. The
#' data contains coordinates projected in EPSG:3035.
#' 
#' @format A dataframe with 3,000 rows and 4 columns:
#' \describe{
#'  \item{\code{id}}{Character vector containing fake IDs demonstrating what real
#'  survey IDs could look like}
#'  \item{\code{income}}{Ordered factor containing fake gross household income data
#'  in groups of 500€ starting from 1000€ and ending at 5000€.}
#'  \item{\code{x}}{Numeric vector containing the x coordinate in meters.}
#'  \item{\code{y}}{Numeric vector containing the y coordinate in meters.}
#' }
#' 
#' @details
#' The household distribution is based on dwelling data from the Census 2022.
#' Households can only be sampled in 100m INSPIRE grid cells with at least
#' one dwelling.
#' 
#' The income distribution is modelled based on Census 2022 data on average
#' cold rent, the share of owner occupiers, and the average living space per
#' inhabitant. Normally distributed noise was added to simulate realistic
#' spatial patterns.
#' 
#' @source 
#' Data is based on the results of the Census 2022.
#' 
#' © Statistische Ämter der Länder und des Bundes, 2024
#' 
#' Available under: \url{https://www.zensus2022.de/EN/Census_results/_inhalt.html}
#' 
#' @examples
#' head(ber_income)
"ber_income"


#' Charging stations in NRW, Germany
#' @description A dataframe containing crowdsourced data on electric charging
#' stations in North Rhine-Westphalia. The data is aggregated to the district
#' level and contains the respective municipality keys (AGS) for each district.
#' 
#' @format A dataframe with 53 rows and 6 columns:
#' \describe{
#'  \item{\code{ags}}{5-digit official municipality key (AGS). The first two
#'  digits represent the state, the third represents the government district,
#'  and the last two represent the district.}
#'  \item{\code{count}}{Number of charging stations per district.}
#'  \item{\code{fee}}{Share of charging stations that require a fee
#'  for using the charging station.}
#'  \item{\code{public}}{Share of public charging stations, i.e., stations
#'  who provide unconditional access.}
#'  \item{\code{capacity}}{Mean number of vehicles that can be charged at
#'  the same time.}
#'  \item{\code{operator}}{Most common charging station operator.}
#' }
#' 
#' @details
#' Charging station locations and details are taken from the OpenStreetMap (OSM)
#' database retrieved through the \href{https://postpass.geofabrik.de/}{Postpass}
#' API. A charging station is either a point or a polygon with the OSM tag
#' `amenity=charging_station`.
#' 
#' @source
#' © OpenStreetMap contributors, ODbL 1.0.
#' \url{https://www.openstreetmap.org/copyright}
#' 
#' @examples
#' head(nrw_chargers)
"nrw_chargers"
