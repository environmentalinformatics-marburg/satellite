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
#' @param coefs coefficients data frame resulting from landsatCoefficients()
#' @param conv conversion type (one of "rad", "ref", "refsun", "bt")
#'
#' @return Raster object with converted values
#'
#' @export landsatCalibration
#' 
#' @references The conversion functions are taken from USGS' Landsat 8 manual
#' which is available online at 
#' \url{http://landsat.usgs.gov/Landsat8_Using_Product.php}
#'
#' @examples
#' not run:
#' landsatCalibration(band, bnbr, coefs, conv = "rad")

landsatCalibration <- function(band, bnbr, coefs, conv = "rad"){
  if(conv == "rad"){
    result <- coefs$RADM[bnbr] * band + coefs$RADA[bnbr]
  } else if(conv == "ref"){
    result <- coefs$REFM[bnbr] * band + coefs$REFA[bnbr]
  } else if(conv == "refsun"){
    result <- (coefs$REFM[bnbr] * band + coefs$REFA[bnbr]) / 
      cos(coefs$SUNZEN[bnbr] * pi / 180.0)
  } else {
    result <- coefs$BTK2[bnbr] / 
      log(coefs$BTK1[bnbr] / (coefs$RADM[bnbr] * band + coefs$RADA[bnbr]) + 1)
  }
  return(result)
}