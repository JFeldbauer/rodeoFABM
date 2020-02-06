#' standard names for dependencie variables in FABM.
#'
#' A dataset containing the standard names of dependency
#' variables from physical host models in FABM.
#'
#' @docType data
#'
#' @usage data(std_names_FABM)
#'
#' @format A data.frame with 38 rows and 3 columns:
#' \describe{
#'   \item{Variable}{name of the standard variable}
#'   \item{Units}{Units of the standard variable}
#'   \item{Corresponding name in CF convention}{the corresponding name in CF convention
#'   \url{http://cfconventions.org/Data/cf-standard-names/28/build/cf-standard-name-table.html}}
#'
#' }
#' @source \url{https://github.com/fabm-model/fabm/wiki/List-of-standard-variables}
"std_names_FABM"

#' Generic hypsographic curve.
#'
#' Hypsographic curve for example lake following a second order polynom
#'
#' @docType data
#'
#' @usage data(hypsograph)
#'
#' @format A text file (.dat) with two columns
#' \describe{
#'   \item{level}{Depth below water surface in m}
#'   \item{Area}{Surface area at depth in m^2}
#'
#' }

"hypsograph"

#' Example meteorological forcing data for GOTM.
#'
#' Meteorologic forcing data for running the GOTM-FABM test example. The data was averaged
#' from different DWD (German Weather Service) stations for the period of 2009 to 2012.
#'
#' @docType data
#'
#' @usage data(meteo_file)
#'
#' @format A text file (.dat) with nine columns
#' \describe{
#'   \item{datetime}{Day of the observation}
#'   \item{Uwind}{North-South wind speed in m/s}
#'   \item{Vwind}{East-West wind speed in m/s}
#'   \item{pressure}{Surface level barometric pressure in pascal}
#'   \item{airTemp}{Air temperature in degree celsius}
#'   \item{relHum}{Relative humidity in percent}
#'   \item{cloudCover}{Cloud cover as decimal fraction}
#'   \item{short}{Shortwave radiation downwelling in W/m^2}
#'   \item{rain}{Daily average precipitation in m/s}
#' }
#' @source \url{https://opendata.dwd.de/climate_environment/CDC/}

"meteo_file"
