#' Compute extraterrestrial solar irradiance (ESun) based on Radiation/Reflection
#'
#' @description
#' Compute extraterrestrial solar irradiance (ESun) using the actual
#' maximum radiation and reflection values within each band.
#' 
#' @param sensor sensor name ("Landsat 8/7/5/4")
#' @param coefs metadata containing sensor informatino e.g. from 
#' \code{\link{landsatMetadata}}
#' @param normalize normalize ESun to mean earth sun distance
#'
#' @return vector object containing ESun for each band
#'
#' @export toaIrradianceRadRef
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
#' The earth sun distance for the normalization is taken from the metadata.
#' 
#' @seealso \code{\link{eSun}} for wrapping this function and alternative 
#' derivation of ESun.
#' 
#' @examples
#' toaIrradianceRadRef(sensor = "Landsat 8", coefs = coefs)
toaIrradianceRadRef <- function(sensor, coefs, normalize = TRUE){
    eSun <- pi * coefs$ESD * coefs$RAD_MAX / coefs$REF_MAX
    if(normalize == TRUE){
      eSun <- 1/coefs$ESD * eSun
    }
}