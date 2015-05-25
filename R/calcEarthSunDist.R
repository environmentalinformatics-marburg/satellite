#' Compute earth-sun distance based on day of the year.
#'
#' @description
#' Earth-sun distance is computed by one of several empirical formulas.
#' 
#' @param date Date of the sensor overpath (YYYY-MM-DD or POSIX* object)
#' @param formula Formula providing author to be used (Spencer, Mather, ESA)
#'
#' @return Vector object containing earth-sun distancen in AU
#'
#' @export calcEartSunDist
#' 
#' @details Computation of earth-sun distance using formulas provided by
#' Spencer (1971), Mather (2005) or ESA.
#' 
#' @references The formulas are taken from the following sources:
#' 
#' Spencer: Spencer JW (1971) Fourier series representation of the position of 
#' the sun. Search 2/5. Taken from 
#' \url{https://goo.gl/lhi9UI}. 
#' 
#' Mather:  Paul M. Mather (2005) Computer Processing of Remotely-Sensed Images:
#' An Introduction. Wiley, ISBN: 978-0-470-02101-9, 
#' \url{http://eu.wiley.com/WileyCDA/WileyTitle/productCd-0470021012.html}.
#' 
#' ESA: ESA Earth Observation Quality Control: Landsat frequently asked questions. 
#' 
#' See also: Bird R, Riordan C (1984) Simple solar spectral model for direct and 
#' diffuse irradiance on horizontal and tilted planes at the Earth's surface for
#' cloudless atmospheres. \url{http://www.nrel.gov/docs/legosti/old/2436.pdf}.
#' 
#' @examples
#' calcEartSunDist(date = "2015-01-01", formula = "Spencer")
#' 
calcEartSunDist <- function(date, formula = "Spencer"){
  day <- as.numeric(strftime(date, format = "%j"))
  if(formula == "Spencer"){
    T <- 2 * pi * (day - 1) / 365
    (1/(1.000110 + 0.034221 * cos(T) + 0.001280 * sin(T) + 0.000719 * 
      cos(2 * T) + 0.000077 * sin(2 * T)))**0.5
  } else if(formula == "Mather"){
    1/(1 - 0.016729 * cos(0.9856 * (day - 4)))
  } else if(formula == "ESA"){
    1 - 0.016729 * cos((2 * pi) * (0.9856 * (day - 4) / 360))
  }
}

