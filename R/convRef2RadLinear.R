#' Convert reflectance to radiance using linear function coefficients
#' 
#' @description 
#' The function converts the reflectance (ref) back to radiance (rad) given that
#' linear conversion coefficients for both radiance and reflectance are 
#' available.
#'
#' @param band raster::RasterStack or raster::RasterLayer containing reflectance.
#' @param refm Multiplication coefficient for reflectance conversion.
#' @param refa Addtition coefficient for reflectance conversion.
#' @param radm Multiplication coefficient for radiance conversion.
#' @param rada Addition coefficient for radiance conversion.
#' @param szen Sun zenith angle.
#'   
#' @return \code{raster::Raster*} object with converted values.
#'
#' @export convRef2RadLinear
#' 
#' @details The conversion functions are taken from USGS' Landsat 8 Data Users
#' Handbook which is available online at 
#' \url{https://www.usgs.gov/core-science-systems/nli/landsat/landsat-8-data-users-handbook}.
#'
convRef2RadLinear <- function(band, refm, refa, radm, rada, szen){
  if(!missing(szen)){
    band <- band * cos(szen * pi / 180.0)
  }
  dn <- (band - refa) / refm
  result <- radm * dn + rada
  return(result)
}