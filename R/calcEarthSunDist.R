#' Compute earth-sun distance based on day of the year
#'
#' @description
#' The earth-sun distance for a particular day of the year is computed based on
#' one of several empirical formulas.
#' 
#' @param date Date of the sensor overpass; either a character string in a 
#' native date format (e.g. "YYYY-MM-DD", see \code{\link{as.Date}}) or a POSIX* 
#' object (see \code{\link{as.POSIXct}}). 
#' @param formula Formula to be applied, specified through the name of the 
#' author, i.e. one of "Spencer", "Mather", "ESA" or "Duffie" (see 'Details').
#'
#' @return Numeric earth-sun distance (in AU) or, if \code{formula = "Duffie"}, 
#' the relative squared earth--sun distance on the given day.
#'
#' @export calcEarthSunDist
#' 
#' @details Computation of earth-sun distance using formulas provided by
#' Spencer (1971), Mather (2005) or ESA. If \code{formula = "Duffie"}, the 
#' inverse squared relative earth--sun distance is returned as proposed by 
#' Duffie and Beckman (1980).
#' 
#' @references The formulas are taken from the following sources:
#' \itemize{
#'   \item Spencer: Spencer JW (1971) Fourier series representation of the position of 
#'   the sun. Search 2/5. Taken from 
#'   \url{https://goo.gl/lhi9UI}. 
#' 
#'   \item Mather: Mather PM (2005) Computer Processing of Remotely-Sensed 
#'   Images: An Introduction. Wiley: Chichester, ISBN: 978-0-470-02101-9, 
#'   \url{http://eu.wiley.com/WileyCDA/WileyTitle/productCd-0470021012.html}.
#' 
#'   \item ESA: ESA Earth Observation Quality Control: Landsat frequently asked questions. 
#'   
#'   \item Duffie: Duffie JA, Beckman WA (2013) Solar Engineering of Thermal 
#'   Processes. Wiley: Hoboken, New Jersey, ISBN: 978-0-470-87366-3,
#'   \url{http://eu.wiley.com/WileyCDA/WileyTitle/productCd-0470873663.html}.
#' }
#' See also: Bird R, Riordan C (1984) Simple solar spectral model for direct and 
#' diffuse irradiance on horizontal and tilted planes at the Earth's surface for
#' cloudless atmospheres. Task No. 3434.10, Solar Energy Research Institute: 
#' Golden, Colorado, \url{http://www.nrel.gov/docs/legosti/old/2436.pdf}.
#' 
#' @examples
#' calcEarthSunDist(date = "2015-01-01", formula = "Spencer") # absolute
#' calcEarthSunDist(date = "2015-01-01", formula = "Duffie")  # relative
#' 
calcEarthSunDist <- function(date, 
                             formula = c("Spencer", "Mather", "ESA", "Duffie")){
  
  ## if not supplied, formula defaults to "Spencer"
  formula <- formula[1]
  
  day <- as.numeric(strftime(date, format = "%j"))
  if(formula == "Spencer"){
    pos <- 2 * pi * (day - 1) / 365
    (1/(1.000110 + 0.034221 * cos(pos) + 0.001280 * sin(pos) + 0.000719 * 
      cos(2 * pos) + 0.000077 * sin(2 * pos)))**0.5
  } else if(formula == "Mather"){
    1/(1 - 0.016729 * cos(0.9856 * (day - 4)))
  } else if(formula == "ESA"){
    1 - 0.016729 * cos((2 * pi) * (0.9856 * (day - 4) / 360))
  } else if (formula == "Duffie") {
    1 + 0.033 * cos(day * 2 * pi / 365)
  } else {
    stop("Please specify a valid 'formula' (see ?calcEarthSunDist).\n")
  }
}

