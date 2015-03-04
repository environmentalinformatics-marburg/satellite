#' Convert reflectance to radiance using linear function coefficients
#'
#' @description
#' The function converts the relfectance (ref) back to radiance (rad) given that
#' linear conversion coefficients for both, radiance and reflectance are 
#' available.
#'
#' @param band raster, rasterstack or data frame object of the sensor band
#' @param coefs coefficients data frame resulting from compMetaLandsat()
#' @param conv conversion type (one of "rad", "ref", "refsun", "bt")
#'
#' @return Raster object with converted values
#'
#' @export calibLinearInverse
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
#' calibLinear(band = getSatDataLayer(sat, bcde),
#'             mult = getSatRADM(sat, bcde),
#'             add = getSatRADA(sat, bcde))
#' 
calibLinearInverse <- function(band, ref_mult, ref_add, 
                               rad_mult, rad_add, szen){
  if(!missing(szen)){
    band <- band * cos(szen * pi / 180.0)
  }
  
  dn <- (band - ref_add) / ref_mult
  result <- rad_mult * dn + rad_add
  return(result)
}