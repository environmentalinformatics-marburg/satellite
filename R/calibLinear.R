#' Convert DNs to radiance, reflectance and/or brightness temperature.
#'
#' @description
#' The function converts the digital numbers of a Lansat 8 level 1B/T file
#' to radiance (rad), relfectance (ref) and/or brightness temperature (bt) using
#' the calibration coefficients from the metadata file. The reflectance 
#' conversion can additionaly be include a sun zenith correction.
#'
#' @param band raster, rasterstack or data frame object of the sensor band
#' @param bnbr number of the band (if rasterstack)
#' @param coefs coefficients data frame resulting from compMetaLandsat()
#' @param conv conversion type (one of "rad", "ref", "refsun", "bt")
#'
#' @return Raster object with converted values
#'
#' @export calibLinear
#' 
#' @references The conversion functions are taken from USGS' Landsat 8 manual
#' which is available online at 
#' \url{http://landsat.usgs.gov/Landsat8_Using_Product.php}

#' @seealso \code{\link{radiometricCorrection}} for converions of the DNs 
#' including scene-based atmospheric correction.
#'
#' @examples
#' path <- system.file("extdata", package = "satellite")
#' filesl8 <- list.files(path, pattern = glob2rx("LC8*.tif"), full.names = TRUE)
#' coefs8 <- compMetaLandsat(filesl8)
#' calibLinear(l8[[2]], 2, coefs8, conv = "ref")

calibLinear <- function(band, bnbr, mult, add, szen, k1, k2){
  result <- mult[bnbr] * band + add[bnbr]
  if(!missing(szen)){
    result <- result / cos(szen[bnbr] * pi / 180.0)
  }
  if(!missing(k1)){
    result <- k2[bnbr] / log(k1[bnbr] / result + 1)
  }
  return(result)
}