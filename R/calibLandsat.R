#' Convert DNs to radiance, reflectance and/or brightness temperature.
#'
#' @description
#' The function converts the digital numbers of a Lansat 8 level 1B/T file
#' to radiance (rad), relfectance (ref) and/or brightness temperature (bt) using
#' the calibration coefficients from the metadata file. The reflectance 
#' conversion can additionaly be include a sun zenith correction.
#'
#' @param band raster object of the Landsat band
#' @param bnbr number of the Landsat band
#' @param coefs coefficients data frame resulting from compMetaLandsat()
#' @param conv conversion type (one of "rad", "ref", "refsun", "bt")
#'
#' @return Raster object with converted values
#'
#' @export calibLandsat
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
#' calibLandsat(l8[[2]], 2, coefs8, conv = "ref")

calibLandsat <- function(band, bnbr, coefs, conv = "rad"){
  if(conv == "rad"){
    result <- coefs$RADM[bnbr] * band + coefs$RADA[bnbr]
  } else if(conv == "ref"){
    if(is.na(coefs$REFM[bnbr])){
      stop(paste0("Missing reflectence correction factors for band ", str(bnbr)))
    }
    result <- coefs$REFM[bnbr] * band + coefs$REFA[bnbr]
  } else if(conv == "refsun"){
    if(is.na(coefs$REFM[bnbr])){
      stop(paste0("Missing reflectence correction factors for band ", str(bnbr)))
    }
    result <- (coefs$REFM[bnbr] * band + coefs$REFA[bnbr]) / 
      cos(coefs$SUNZEN[bnbr] * pi / 180.0)
  } else {
    if(is.na(coefs$BTK2[bnbr])){
      stop(paste0("Missing temperature correction factors for band ", str(bnbr)))
    }
    result <- coefs$BTK2[bnbr] / 
      log(coefs$BTK1[bnbr] / (coefs$RADM[bnbr] * band + coefs$RADA[bnbr]) + 1)
  }
  return(result)
}