#' Compute extraterrestrial solar irradiance (ESun) for satellite bands
#'
#' @description
#' Compute extraterrestrial solar irradiance (ESun) using mean solar spectral 
#' data and the band, sun-earth distance based on the day of the year and band
#' specifiv relative spectral response functions (rsr).
#' 
#' @param sensor sensor name ("Landsat 8/7/5/4")
#' @param date date of the sensor overpath (YYYY-MM-DD or POSIX* object)
#' @param model tabulated solar radiation model to be used 
#' @param radref use radiance/reflectance ratio (Landsat 8 only, see details)
#' @param coefs Landsat 8 metadata from \code{\link{landsatCoefficients}}
#' (Landsat 8 only, see details)
#'
#' @return vector object containing ESun for each band
#'
#' @export toaIrradiance
#' 
#' @details Computation of ESun is taken from Updike and Comp (2011). Sun-earth
#' distance is computed using \code{\link{earthSun}}.
#' 
#' If sensor is Landas 8 and radref = TRUE, eSun computes the actual solar 
#' irradiance using the following formula taken from GRASS' 
#' i.landsat.toar module
#' (\url{http://grass.osgeo.org/grass65/manuals/i.landsat.toar.html}):
#' \deqn{ESun = (pi d^2) RADIANCE_MAXIMUM / REFLECTANCE_MAXIMUM}
#' where d is the sun-earth distance in astronomical units and RADIANCE_MAXIMUM,
#' REFLECTANCE_MAXIMUM are the maximum radiance and reflection values of the
#' respective band. All these parameters are taken from the scene's metadata
#' file. 
#' 
#' @references Computation of ESun is taken from Updike T, Comp C (2011) 
#' Radiometric use of WorldView-2 imagery. Technical Note, URL 
#' \URL{https://www.digitalglobe.com/sites/default/files/Radiometric_Use_of_WorldView-2_Imagery(1).pdf}.
#' 
#' Tabulated relative spectral response functions (nm-1) are taken from the USGS
#' at \url{http://landsat.usgs.gov/instructions.php}.
#' 
#' Tabulated solar irradiance (W m-2 nm-1) are taken from the National Renewable
#' Energy Laboratory at \url{http://rredc.nrel.gov/solar/spectra/am0/modtran.html}.
#' 
#' @seealso \code{\link{earthSun}} for calculating the sun-earth distance based
#' on the day of the year and \code{\link{eSun}} for wrapping this function and
#' alternative derivation of ESun.
#' 
#' @examples
#' toaIrradiance(sensor = "Landsat 8")
toaIrradiance <- function(sensor, date, model = "MNewKur", radref = FALSE, 
                          coefs){
  if(sensor == "Landsat 8" & simple == TRUE){
    if(missing(coefs)){
      stop("Landsat 8 scene coefficients are missing.")
    }
    esd <- earthSun(date)
    pi * esd * coefs$RAD_MAX / coefs$REF_MAX
  } else {
    if(sensor == "Landsat 8"){rsr <- lut$l8_rsr}
    else if(sensor == "Landsat 7"){rsr <- lut$l7_rsr}
    else{stop("Only Landsat 7 and 8 are implemented, yet.")}
    
    toa <- solar
    toa$Wavelength <- round(toa$Wavelength, 0)
    toa_aggregated <- aggregate(toa, by = list(toa$Wavelength), FUN = "mean")

    rsr_solar <- merge(rsr, toa_aggregated, by = "Wavelength")
    
    
    eSun <- aggregate(rsr_solar$RSR * rsr_solar[,grep(model, names(rsr_solar))], 
                     by = list(rsr_solar$Band), FUN = "sum") /
      aggregate(rsr_solar$RSR, by = list(rsr_solar$Band), FUN = "sum") * 1000
    eSun <- unlist(eSun[2])
    names(eSun) <- paste0("Band_", seq(11))
  }
}