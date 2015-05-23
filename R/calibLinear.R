#' Convert DNs to radiance, reflectance and/or brightness temperature.
#'
#' @description
#' The function converts the digital numbers of a Lansat 8 level 1B/T file
#' to radiance (rad), relfectance (ref) and/or brightness temperature (bt) using
#' the calibration coefficients from the metadata file. The reflectance 
#' conversion can additionaly be include a sun zenith correction.
#'
#' @param band raster, rasterstack or data frame object of the sensor band
#' @param mult multiplicative coefficient for value transformation (i.e. slope)
#' @param add additive coefficient for value transformation (i.e. offset)
#' @param szen cosine of solar zenith angle
#' @param k1,k2 temperature correction parameters
#'
#' @return Raster object with converted values
#'
#' @export convertLinear
#' 
#' @references The conversion functions are taken from USGS' Landsat 8 manual
#' which is available online at 
#' \url{http://landsat.usgs.gov/Landsat8_Using_Product.php}

#' @seealso \code{\link{radiometricCorrection}} for converions of the DNs 
#' including scene-based atmospheric correction.
#'
#' @examples
#' path <- system.file("extdata", package = "satellite")
#' files <- list.files(path, pattern = glob2rx("LC8*.tif"), full.names = TRUE)
#' sat <- satellite(files)
#' 
#' bcde <- "002n"
#' 
#' convertLinear(band = getSatDataLayer(sat, bcde),
#'             mult = getSatRADM(sat, bcde),
#'             add = getSatRADA(sat, bcde))
#' 
convertLinear <- function(band, mult, add, szen, k1, k2){
  result <- mult * band + add
  if(!missing(szen)){
    result <- result / cos(szen * pi / 180.0)
  }
  if(!missing(k1)){
    result <- k2 / log(k1 / result + 1)
  }
  return(result)
}