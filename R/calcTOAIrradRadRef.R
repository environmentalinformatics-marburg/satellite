#' Compute extraterrestrial solar irradiance (ESun) based on Radiation/Reflection
#'
#' @description
#' Compute extraterrestrial solar irradiance (ESun) using the actual
#' maximum radiation and reflection values within each band.
#' 
#' @param rad_max maximum radiance of satellite band(s)
#' @param rad_min minimum radiance of satellite band(s)
#' @param esd earth-sun distance (AU, can be estimated using 
#' \code{\link{calcEartSunDist}})
#' @param normalize normalize ESun to mean earth sun distance
#'
#' @return vector object containing ESun for each band
#'
#' @export calcTOAIrradRadRef
#' 
#' @details The actual solar irradiance is compute using the following formula 
#' taken from GRASS' 
#' \href{http://grass.osgeo.org/grass65/manuals/i.landsat.toar.html}{i.landsat.toar module}
#' \deqn{ESun = (pi d^2) RADIANCE_MAXIMUM / REFLECTANCE_MAXIMUM}
#' where d is the sun-earth distance in astronomical units and RADIANCE_MAXIMUM,
#' REFLECTANCE_MAXIMUM are the maximum radiance and reflection values of the
#' respective band. All these parameters are taken from the scene's metadata
#' file.
#' 
#' By default, the resulting actual ESun will be normalized to a mean earth sun 
#' distance to be compatible with other default results from \code{\link{eSun}}.
#' 
#' @seealso \code{\link{calcTOAIrradRadTable}} for tabulated solar irradiance
#' values from the literature or \code{\link{calcTOAIrradModel}} for the 
#' computation of the solar irradiance based on look-up tables for the sensor's 
#' relative spectral resonse and solar irradiation spectral data.
#' 
#' See \code{\link{calcEartSunDist}} for calculating the sun-earth 
#' distance based on the day of the year which is called by this function if
#' ESun should be corrected for actual earth sun distance.
#' 
#' See \code{\link{satTOAIrrad}} which can be used as a wrapper function if the
#' data is organized as a Satellite object.
#'  
#' @examples
#' calcTOAIrradRadRef(coefs = coefs)
calcTOAIrradRadRef <- function(rad_max, ref_max, normalize = TRUE, esd){
  eSun <- pi * esd * rad_max / ref_max
  if(normalize == TRUE){
    eSun <- 1/esd * eSun
  }
  return(eSun)
}