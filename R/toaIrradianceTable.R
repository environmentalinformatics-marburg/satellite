#' Get extraterrestrial solar irradiance (ESun) from readily tabulated values
#'
#' @description
#' Get mean extraterrestrial solar irradiance (ESun) using publisch values.
#' 
#' @param sensor sensor name ("Landsat 8/7")
#' @param normalize normalize ESun to mean earth sun distance
#' @param date date of the sensor overpath (YYYY-MM-DD or POSIX* object) (
#' only required if ESun should be corrected to the actual earth sun distance)
#'
#' @return vector object containing ESun for each band
#'
#' @export toaIrradianceTable
#' 
#' @details Computation of ESun is taken from Updike and Comp (2011). Sun-earth
#' distance is computed using \code{\link{earthSun}}.
#' 
#' @references Tabulated values of ESun are taken from
#' 
#' Landsat 7: Landsat7 handbook, tab 11.3 (Thuillier spectrum), URL 
#' \url{http://landsathandbook.gsfc.nasa.gov/pdfs/Landsat7_Handbook.pdf}.
#' 
#' Landsat 4, 5: Chander G, Markham B (2003) Revised Landsat-5 TM radiometric 
#' calibration procedures and postcalibration dynamic ranges.  IEEE Transaction 
#' on Geoscience and Remote Sensing 41/11, doi:10.1109/LGRS.2007.898285, URL
#' \url{http://landsathandbook.gsfc.nasa.gov/pdfs/L5TMLUTIEEE2003.pdf}.
#' 
#' @seealso \code{\link{eSun}} for wrapping this function and alternative 
#' derivation of ESun and \code{\link{earthSun}} for calculating the sun-earth 
#' distance based on the day of the year which is called by this function if
#' ESun should be corrected for actual earth sun distance.
#' 
#' @examples
#' toaIrradianceTable(sensor = "Landsat 7", normalize = FALSE, "2015-01-01")
#' 
toaIrradianceTable <- function(sensor, normalize = TRUE, date){
  if(sensor == "Landsat 7") {
    eSun <- lut$l7_esun
  } else if(sensor == "Landsat 5") {
    eSun <- lut$l5_esun
  } else if(sensor == "Landsat 4") {
    eSun <- lut$l4_esun
  }
  if(normalize == FALSE){
    if(missing(date)){
      stop("Variable date is missing.")
    }
    esd <- earthSun(date)
    eSun <- esd * ESun
  }
  return(eSun)
}