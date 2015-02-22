#' Get internal LUT values used by various functions
#'
#' @description
#' Get internal LUT values from sysdata.rda which have been compiled using
#' data-raw/lut_data.R. Metadata is stored in lut$meta.
#'
#' @return List containing several data frames with LUT values
#'
#' @export lutInfo
#' 
#' @details The LUT contains the following information:
#' \describe{
#'   \item{l4_band_wl}{Minimum/maximum wavelength for Landsat 4 bands taken from the
#' \href{http://landsat.usgs.gov/band_designations_landsat_satellites.php}{band info}
#' of the }
#'   \item{l5_band_wl}{ Minimum/maximum wavelength for Landsat 5 bands taken from the
#' \href{http://landsat.usgs.gov/band_designations_landsat_satellites.php}{band info}
#' of the USGS Landsat FAQ.}
#'   \item{l7_band_wl}{Minimum/maximum wavelength for Landsat 7 bands taken from the
#' \href{http://landsat.usgs.gov/band_designations_landsat_satellites.php}{band info}
#' of the USGS Landsat FAQ.}
#' \item{l8_band_wl}{Minimum/maximum wavelength for Landsat 8 bandstaken from the
#' \href{http://landsat.usgs.gov/band_designations_landsat_satellites.php}{band info}
#' of the USGS Landsat FAQ.}
#' \item{l7_rsr}{Landat 7 relative spectral response (nm-1) taken from taken from the
#' \href{http://landsat.usgs.gov/instructions.php}{spectral viewer}
#' of the USGS Landsat FAQ.}
#' \item{l8_rsr}{Landat 8 relative spectral response (nm-1) taken from taken from the
#' \href{http://landsat.usgs.gov/instructions.php}{spectral viewer}
#' of the USGS Landsat FAQ.}
#' \item{solar}{Solar irradiance (W m-2 nm-1) taken from the 
#' \href{http://rredc.nrel.gov/solar/spectra/am0/modtran.html}{National Renewable 
#' Energy Laboratory}.}
#' \item{l7_esun}{Tabulated ESun values from 
#' \href{from http://landsathandbook.gsfc.nasa.gov/pdfs/Landsat7_Handbook.pdf}{tab 11.3 (Thuillier spectrum)}
#'  of the Landsat7 handbook.}
#' \item{l5_esun}{Tabulated ESun values from 
#' \href{http://landsathandbook.gsfc.nasa.gov/pdfs/L5TMLUTIEEE2003.pdf}{Chander
#' G, Markham B (2003) Revised Landsat-5 TM radiometric calibration procedures 
#' and postcalibration dynamic ranges.  IEEE Transaction on Geoscience and 
#' Remote Sensing 41/11, doi:10.1109/LGRS.2007.898285}}
#' \item{l4_esun}{Tabulated ESun values from 
#' \href{http://landsathandbook.gsfc.nasa.gov/pdfs/L5TMLUTIEEE2003.pdf}{Chander
#' G, Markham B (2003) Revised Landsat-5 TM radiometric calibration procedures 
#' and postcalibration dynamic ranges.  IEEE Transaction on Geoscience and 
#' Remote Sensing 41/11, doi:10.1109/LGRS.2007.898285}}
#' }
#'  
#' @examples
#' lutInfo()
#' 
lutInfo <- function(...){
  return(lut)
}