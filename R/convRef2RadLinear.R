#' Convert reflectance to radiance using linear function coefficients
#' 
#' @description 
#' The function converts the relfectance (ref) back to radiance (rad) given that
#' linear conversion coefficients for both, radiance and reflectance are 
#' available.
#'
#' @param band raster::RasterStack or raster::RasterLayer containing reflectance
#' @param REFM multiplication coefficient for reflectance conversion
#' @param REFA addtition coefficient for reflectance conversion
#' @param RADM multiplication coefficient for radiance conversion
#' @param RADA addtition coefficient for radiance conversion
#' @param SZEN sun zenith angle
#'   
#' @return Raster object with converted values
#'
#' @export convRef2RadLinear
#' 
#' @details The conversion functions are taken from USGS' Landsat 8 manual
#' which is available online at 
#' \url{http://landsat.usgs.gov/Landsat8_Using_Product.php}
#'
#' @examples
#' Not run:
#' 
convRef2RadLinear <- function(band, refm, refa, radm, rada, szen){
  if(!missing(szen)){
    band <- band * cos(szen * pi / 180.0)
  }
  dn <- (band - refa) / refm
  result <- radm * dn + rada
  return(result)
}