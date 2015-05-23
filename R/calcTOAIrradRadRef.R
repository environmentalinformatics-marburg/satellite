if ( !isGeneric("calcTOAIrradRadRef") ) {
  setGeneric("calcTOAIrradRadRef", function(x, ...)
    standardGeneric("calcTOAIrradRadRef"))
}
#' Compute top of atmosphere solar irradiance using radiation vs. reflection
#'
#' @description
#' Compute extraterrestrial solar irradiance (ESun) using the actual
#' maximum radiation and reflection values within each band.
#' 
#' @param x An object of type Satellite or the maximum radiance of satellite 
#' band(s) as numeric object
#' @param rad_min minimum radiance of satellite band(s)
#' @param normalize Normalize ESun to mean earth sun distance, TRUE or FALSE
#' @param esd Earth-sun distance (AU, can be estimated using 
#' \code{\link{calcEarthSunDist}}). If x is a Satellite object and esd is not 
#' supplied and necessary for normalization, it is tried to take it from the 
#' metadata, otherwise it is estimated by the day of the year using 
#' \code{\link{calcEartSunDist}}.
#'
#' @export calcTOAIrradRadRef
#' 
#' @name calcTOAIrradRadRef
#' 
#' @details The actual solar irradiance is compute using the following formula 
#' taken from GRASS GIS' 
#' \href{http://grass.osgeo.org/grass65/manuals/i.landsat.toar.html}{i.landsat.toar module}
#' \deqn{ESun = (pi d^2) RADIANCE_MAXIMUM / REFLECTANCE_MAXIMUM}
#' where d is the sun-earth distance in astronomical units and RADIANCE_MAXIMUM
#' and REFLECTANCE_MAXIMUM are the maximum radiance and reflection values of the
#' respective band. All these parameters are taken from the scene's metadata
#' file if a Satellite object is passed to the function.
#' 
#' By default, the resulting actual ESun will be normalized to a mean earth sun 
#' distance to be compatible with other default results from \code{\link{eSun}}.
#' 
#' @seealso \code{\link{calcTOAIrradTable}} for tabulated solar irradiance
#' values from the literature or \code{\link{calcTOAIrradModel}} for the 
#' computation of the solar irradiance based on look-up tables for the sensor's 
#' relative spectral resonse and solar irradiation spectral data.
#' 
#' See \code{\link{calcEarthSunDist}} for calculating the earth-sun
#' distance based on the day of the year which is called by this function if
#' ESun should be corrected for actual earth sun distance.
#'  
#' @examples
#' path <- system.file("extdata", package = "satellite")
#' files <- list.files(path, pattern = glob2rx("LC8*.tif"), full.names = TRUE)
#' sat <- satellite(files)  
#' sat <- calcTOAIrradModel(sat)
#' getSatESUN(sat)
#' 
#' calcTOAIrradRadRef(x = getSatRadMax(x, getSatBCDESolar(x)), 
#'                    ref_max = getSatRefMax(x, getSatBCDESolar(x)), 
#'                    normalize = normalize, 
#'                    esd = calcEartSunDist("2015-01-01"))
#'                    
NULL

# Function using satellite object ----------------------------------------------
#' 
#' @return Satellite object with ESun information added to the metadata
#' 
#' @rdname calcTOAIrradRadRef
#'
setMethod("calcTOAIrradRadRef", 
          signature(x = "Satellite"), 
          function(x, normalize = TRUE, esd){
            if(normalize == TRUE & missing(esd)){
              esd = getSatESD(x)
              if(is.na(esd)){
                esd = calcEartSunDist(date)
              } 
            }
            if(normalize == TRUE){
              esun <- calcTOAIrradRadRef(
                x = getSatRadMax(x, getSatBCDESolar(x)), 
                ref_max = getSatRefMax(x, getSatBCDESolar(x)),
                esd = esd, normalize = normalize)
            } else {
              esun <- 
                calcTOAIrradRadRef(
                  x = getSatRadMax(x, getSatBCDESolar(x)), 
                  ref_max = getSatRefMax(x, getSatBCDESolar(x)), 
                  normalize = normalize)
            }
            bcde = getSatBCDESolar(x)
            x <- addSatMetaParam(x, 
                                 meta_param = data.frame(
                                   BCDE = bcde,
                                   ESUN = as.numeric(esun)))
            return(x)
          })


# Function using numeric -------------------------------------------------------
#' 
#' @return Vector object containing ESun for the respective band(s)
#' 
#' @rdname calcTOAIrradRadRef
#'
setMethod("calcTOAIrradRadRef", 
          signature(x = "numeric"), 
          function(x, ref_max, normalize = TRUE, esd){
            eSun <- pi * esd * x / ref_max
            if(normalize == TRUE){
              eSun <- 1/esd * eSun
            }
            return(eSun)
          })