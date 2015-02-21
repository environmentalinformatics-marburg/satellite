#' Get extraterrestrial solar irradiance (ESun) for satellite bands
#'
#' @description
#' Get extraterrestrial solar irradiance (ESun) from tabulated or computed
#' values. If values are computed, mean solar spectral data and the band 
#' specifiv relative spectral response functions (rsr) are used.
#' 
#' For Landsat 8, no tabulated values are availabe. Instead (and if tab = TRUE)
#' ESun will be calculated based on the actual maximum radiance and reflection 
#' given in the metadata file. Otherwise they will be computed using the 
#' approach discribed above.
#'
#' @param sensor sensor name ("Landsat 8/7/5/4")
#' @param tab use tabulated values (TRUE) or compute values based on rsr (FALSE)
#' @param date date of the sensor overpath (YYYY-MM-DD or POSIX* object), only 
#' relevant if tab = FALSE or sensor = "Landsat 8"
#' @param coefs Landsat 8 metadata from \code{\link{landsatCoefficients}}
#' (Landsat 8 only, see details)
#'
#' @return vector object containing ESun for each band
#'
#' @export eSun
#' 
#' @details Instead of returning tabulated values for Landsat 8, eSun computes 
#' the actual solar irradiance using the following formula taken from GRASS' 
#' i.landsat.toar module
#' (\url{http://grass.osgeo.org/grass65/manuals/i.landsat.toar.html}):
#' \deqn{ESun = (pi d^2) RADIANCE_MAXIMUM / REFLECTANCE_MAXIMUM}
#' where d is the sun-earth distance in astronomical units and RADIANCE_MAXIMUM,
#' REFLECTANCE_MAXIMUM are the maximum radiance and reflection values of the
#' respective band. All these parameters are taken from the scene's metadata
#' file. 
#' 
#' If ESun should be computed (all sensors), \code{\link{toaIrradiance}} will be
#' called by this function.
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
#' @seealso \code{\link{toaIrradiance}} for computing ESun.
#' 
#' @examples
#' eSun(sensor = "Landsat 8")
eSun <- function(sensor, tab = TRUE, date, coefs){
  if(tab == TRUE){
    if(sensor == "Landsat 8"){
      if(missing(date) | missing(coefs)){
        stop("Variables date or coefs are missing.")
      }
      toaIrradiance(sensor = sensor, date = date, radref = TRUE,
                                            coefs = coefs)
    } else if(sensor == "Landsat 7") {
      eSun <- lut$l7_esun
    } else if(sensor == "Landsat 5") {
      eSun <- lut$l5_esun
    } else if(sensor == "Landsat 4") {
      eSun <- lut$l4_esun
    }
  } else {
    if(missing(date)){
      stop("Variable date is missing.")
    }
    toaIrradiance(sensor = sensor, date = date)
  }
}