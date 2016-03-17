#' Compute declination angle
#' 
#' @description 
#' Compute the declination of the sun at solar noon based on the day of the 
#' year.
#' 
#' @param n Day of the year (DOY) as \code{integer} or \code{Date} from which to
#' derive DOY.
#' @param formula Formula to be applied, specified through the name of the 
#' author (see References). Currently, the approximation equation of Cooper 
#' (1969) and the more accurate estimation of Spencer (1971) are implemented.
#' 
#' @return 
#' A \code{numeric} declination angle.
#' 
#' @details 
#' "The declination of the sun is the angle between the equator and a line drawn 
#' from the centre of the Earth to the centre of the sun." (directly taken from 
#' \url{http://www.pveducation.org/pvcdrom/properties-of-sunlight/declination-angle})
#' 
#' @references 
#' The formulas are taken from the following sources:
#' \itemize{
#'   \item \strong{Cooper}: Cooper PI (1969) The absorption of radiation in 
#'   solar stills. Solar Energy 12(3), 333--346, 
#'   \url{http://www.sciencedirect.com/science/article/pii/0038092X69900474}.
#'   \item \strong{Spencer}: Spencer JW (1971) Fourier series representation of 
#'   the position of the sun. Search 2(5), 172, \url{https://goo.gl/lhi9UI}.
#' }
#' See also 
#' \itemize{
#'   \item Duffie JA, Beckman WA (2013) Solar Engineering of Thermal Processes. 
#'   Wiley: Hoboken, New Jersey, ISBN: 978-0-470-87366-3,
#'   \url{http://eu.wiley.com/WileyCDA/WileyTitle/productCd-0470873663.html} and 
#'   \item ITACA (2016) Part 3: Calculating Solar Angles, 
#'   \url{http://www.itacanet.org/the-sun-as-a-source-of-energy/part-3-calculating-solar-angles/}
#' }
#' for further information.
#' 
#' @examples 
#' declination(Sys.Date())
#' declination(Sys.Date(), formula = "Spencer")
#' 
#' @export declination
#' @name declination
declination <- function(n, formula = c("Cooper", "Spencer")) {
  
  ## if 'Date' input, retrieve day of the year
  if (class(n) == "Date")
    n <- as.integer(strftime(n, format = "%j"))
  
  ## formula by Cooper 1969  
  if (formula[1] == "Cooper") {
    23.45 * sin(2 * pi * (284 + n) / 365)
    
  ## formula by Spencer 1971  
  } else if (formula[1] == "Spencer") {
    B <- (n - 1) * 2 * pi / 365
    (180 / pi) * (0.006918 - 0.399912 * cos(B) + 0.070257 * sin(B) - 
                    0.006758 * cos(2 * B) + 0.000907 * sin(2 * B) - 
                    0.002697 * cos(3 * B) + 0.00148 * sin(3 * B))
    
  ## stop if no valid formula is specified  
  } else {
    stop("Please specify a valid 'formula' argument (see ?declination).\n")
  }
}