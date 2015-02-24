#' Get extraterrestrial solar irradiance (ESun) from readily tabulated values
#'
#' @description
#' Get mean extraterrestrial solar irradiance (ESun) using published values.
#' 
#' @param sensor sensor name ("Landsat 4/5/7")
#' @param normalize normalize ESun to mean earth sun distance
#' @param date date of the sensor overpath (YYYY-MM-DD or POSIX* object) (
#' only required if ESun should be corrected to the actual earth sun distance)
#'
#' @return Vector object containing ESun for each band
#'
#' @export calcTOAIrradianceTable
#' 
#' @details Currently implemented sensors are Landsat 4, 5 and 7.
#' 
#' If results should not be normalized to a mean earth sun distance, the 
#' actual earth sun distance is approximated by the day of the year using
#' \code{\link{earthSun}}.
#' 
#' @references  Tabulated values of the solar irradiance for Landsat 4 and 5 are 
#' taken from Chander G, Markham B (2003) Revised Landsat-5 TM radiometric 
#' calibration procedures and postcalibration dynamic ranges.  IEEE Transaction 
#' on Geoscience and Remote Sensing 41/11, doi:10.1109/LGRS.2007.898285, URL
#' \url{http://landsathandbook.gsfc.nasa.gov/pdfs/L5TMLUTIEEE2003.pdf}.
#' 
#' Tabulated values of the solar irradiance for Landsat 7 are taken from
#' \href{http://landsathandbook.gsfc.nasa.gov/pdfs/Landsat7_Handbook.pdf}{NASA's
#' Landsat7 handbook, tab 11.3 (Thuillier spectrum)}
#' 
#' @seealso \code{\link{calcTOAIrradianceTable}} for tabulated solar irradiance
#' values from the literature or \code{\link{calcTOAIrradianceRadRef}} for the 
#' computation of the solar irradiance based on maximum radiation and reflection
#' values of the dataset.
#' 
#' See \code{\link{earthSun}} for calculating the sun-earth 
#' distance based on the day of the year which is called by this function if
#' ESun should be corrected for actual earth sun distance.
#' 
#' See \code{\link{eSun}} which can be used as a wrapper function for the
#' satellite sensors already included in this package.
#' 
#' @examples
#' calcTOAIrradianceTable(sensor = "Landsat 7", normalize = FALSE, "2015-01-01")
#' 
calcTOAIrradianceTable <- function(sensor, normalize = TRUE, date){
  if(sensor == "Landsat 7") {
    eSun <- lut$L7_ESUN
  } else if(sensor == "Landsat 5") {
    eSun <- lut$L5_ESUN
  } else if(sensor == "Landsat 4") {
    eSun <- lut$L4_ESUN
  }
  if(normalize == FALSE){
    if(missing(date)){
      stop("Variable date is missing.")
    }
    esd <- earthSun(date)
    eSun <- esd * eSun
  }
  return(eSun)
}