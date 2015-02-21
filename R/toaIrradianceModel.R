#' Compute extraterrestrial solar irradiance (ESun) based on RSR
#'
#' @description
#' Compute mean extraterrestrial solar irradiance (ESun) using tabulated mean
#' solar spectral data and the band specifiv relative spectral response 
#' functions (rsr).
#' 
#' @param sensor sensor name ("Landsat 8/7")
#' @param model tabulated solar radiation model to be used 
#' @param normalize normalize ESun to mean earth sun distance
#' @param date date of the sensor overpath (YYYY-MM-DD or POSIX* object) (
#' only required if ESun should be corrected to the actual earth sun distance)
#'
#' @return vector object containing ESun for each band
#'
#' @export toaIrradianceModel
#' 
#' @details Computation of ESun is taken from Updike and Comp (2011). Sun-earth
#' distance is computed using \code{\link{earthSun}}.
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
#' @seealso \code{\link{eSun}} for wrapping this function and alternative 
#' derivation of ESun and \code{\link{earthSun}} for calculating the sun-earth 
#' distance based on the day of the year which is called by this function if
#' ESun should be corrected for actual earth sun distance.
#' 
#' @examples
#' toaIrradianceModel(sensor = "Landsat 7", model = "MNewKur")
#' 
toaIrradianceModel <- function(sensor, model = "MNewKur", 
                               normalize = TRUE, date){
  if(sensor == "Landsat 8"){
    rsr <- lut$l8_rsr
  } else if(sensor == "Landsat 7"){
    rsr <- lut$l7_rsr
  } else{
    stop("Only Landsat 7 and 8 are implemented, yet.")
  }
  
  toa <- lut$solar
  toa$Wavelength <- round(toa$Wavelength, 0)
  toa_aggregated <- aggregate(toa, by = list(toa$Wavelength), FUN = "mean")
  
  rsr_solar <- merge(rsr, toa_aggregated, by = "Wavelength")
  
  
  eSun <- aggregate(rsr_solar$RSR * rsr_solar[,grep(model, names(rsr_solar))], 
                    by = list(rsr_solar$Band), FUN = "sum") /
    aggregate(rsr_solar$RSR, by = list(rsr_solar$Band), FUN = "sum") * 1000
  eSun <- unlist(eSun[2])
  names(eSun) <- paste0("Band_", seq(length(eSun)))
  
  if(normalize == FALSE){
    if(missing(date)){
      stop("Variable date is missing.")
    }
    esd <- earthSun(date)
    eSun <- esd * eSun
  }
  
  return(eSun)
}