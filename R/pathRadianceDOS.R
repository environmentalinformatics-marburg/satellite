#' Compute path radiance based on dark object method
#'
#' @description
#' Compute an estimaed path radiance for all sensor band_wls using a dark object 
#' method which can be used to roughly correct the radiance values for 
#' atmospheric scattering.
#'
#' @param DNmin digital number of dark object in band bnbr
#' @param bnbr band number for which DNmin is valid
#' @param band_wls band wavelengths for which correction should be made
#' @param coefs metadata from \code{\link{landsatMetadata}}
#' @param model to be used to correct for 1% scattering (DOS2, DOS4; must be the
#' same as used by \code{\link{radiometricCorrection}})
#' @param ESun normalized extraterrestrial solar irradiance for all band_wls
#' (W m-2 micrometer -1) which can be computed by  \code{\link{eSun}})
#' @param scat_coef scattering coefficient (-4.0, -2.0, -1.0, -0.7, -0.5)
#' @param dos_adjust dark object adjustment assuming a reflexion of e.g. 0.01
#'  
#' @return Vector object with path radiance values for each band 
#' (W m-2 micrometer-1)
#'
#' @export pathRadianceDOS
#' 
#' @details The dark object substraction approach is based on an approximation 
#' of the atmospheric path radiance (i.e. upwelling radiation which is 
#' scatter into the sensors field of view, aka haze) using the reflectance of a 
#' dark object (i.e. reflectance ~1%). 
#' 
#' Chavez (1988) proposed a method which uses the dark object reflectance
#' in one band to predict the corresponding path radiances in all other band_wls 
#' using a relative radiance model which depends on the wavlelength and overall
#' atmospheric optical thickness (which is estimated based on the dark object's
#' DN value). This has the advantage that the path radiance is actually 
#' correlated across different sensor band_wls and not computed individuall for 
#' each band using independent dark objects. He proposed a relative radiance 
#' model which follows a wavelength dependent scattering that ranges from a
#' power of -4 over -2, -1, -0.7 to -0.5 for very clear over clear, moderate, 
#' hazy to very hazy conditions. The relative factors are computed individually
#' for each 1/1000 wavelength within each band range and subsequently averaged 
#' over the band as proposed by Goslee (2011).
#' 
#' The atmospheric transmittance towards the sensor (Tv) is approximated by 
#' 1.0 (DOS2, Chavez 1996) or rayleigh scattering (DOS4, Moran et al. 1992)
#' 
#' The atmospheric transmittance from the sun (Tz) is approximated by the 
#' cosine of the sun zenith angle (DOS2, Chavez 1996) or again using rayleigh
#' scattering (DOS4, Moran et al. 1992).
#' 
#' The downwelling diffuse irradiance is approximated by 0.0 (DOS2, Chavez 1996)
#' or the hemispherical integral of the path radiance (DOS4, Moran et al. 1992).
#' 
#' Equations for the path radiance are taken from Song et al. (2001).
#' 
#' @references Chavez Jr PS (1988) An improved dark-object subtraction technique 
#' for atmospheric scattering correction of multispectral data. Remote Sensing 
#' of Environment 24/3, doi:10.1016/0034-4257(88)90019-3, available online at
#'  \url{http://www.sciencedirect.com/science/article/pii/0034425788900193}
#'  
#' Chavez Jr PS (1996) Image-based atmospheric corrections revisited and
#' improved. Photogrammetric Engineering and Remote Sensing 62/9,
#' available online at 
#' \url{http://www.asprs.org/PE-RS-Journals-1996/PE-RS-September-1996.html}
#'  
#' Goslee SC (2011) Analyzing Remote Sensing Data in R: The landsat 
#' Package. Journal of Statistical Software,43/4, 1-25. URL 
#' \url{http://www.jstatsoft.org/v43/i04/}.
#' 
#' Moran MS, Jackson RD, Slater PN, Teillet PM (1992) Evlauation of simplified
#' procedures for rretrieval of land surface reflectane factors from satellite
#' sensor output.Remote Sensing of Environment 41/2-3, 169-184, 
#' doi:10.1016/0034-4257(92)90076-V, 
#' URL \url{http://www.sciencedirect.com/science/article/pii/003442579290076V}.
#' 
#' Song C, Woodcock CE, Seto KC, Lenney MP, Macomber SA (2001) Classification 
#' and Change Detection Using Landsat TM Data: When and How to Correct 
#' Atmospheric Effects? Remote Sensing of Environment 75/2, 
#' doi:10.1016/S0034-4257(00)00169-3, URL
#' \url{http://www.sciencedirect.com/science/article/pii/S0034425700001693}
#'
#' If you refer to Sawyer and Stephen 2014, please note that eq. 5 is wrong.
#' 
#' @seealso This function is used by \code{\link{radiometricCorrection}} to 
#' compute the path radiance.
#' 
#' @examples
#'   #Example for Landat 8
#'   landsat8_metadatafile <-   system.file("extdata", 
#'   "LC81950252013188LGN00_MTL.txt", package = "satellite")
#'   coefs8 <- landsatMetadata(landsat8_metadatafile)
#'   
#'   pathRadianceDOS(DNmin = min(raster::getValues(l8[[2]])), 
#'   bnbr = 2, band_wls = l8_band_wl, coefs = coefs8,
#'   eSun(sensor = "Landsat 8", tab = TRUE, coefs = coefs8), 
#'   scat_coef = -4)
#'   
pathRadianceDOS <- function(DNmin, bnbr, band_wls, coefs, model = "DOS2", 
                            ESun, scat_coef = -4.0, dos_adjust = 0.01){
  
  # Define relative scattering model based on wavelength dependent scattering
  # processes and different atmospheric optical thiknesses. Resulting 
  # coefficients are the band wavelength to the power of the respective 
  # scattering coefficients defined for the model.
  scattering_model <- sapply(seq(nrow(band_wls)), function(x){
    act_band <- seq(band_wls[x, 1], band_wls[x, 2], by=0.001)
    mean(act_band ^ scat_coef)
  })
  
  # Compute multiplication factors which relate the scattering model coefficents
  # between the individual band_wls and the one band which has been used for the
  # extraction of the dark object radiation.
  basline_factor <- scattering_model[bnbr]
  scattering_factors <- scattering_model / basline_factor
  
  # Compute radiation multiplication factors for radiance conversion (i.e.
  # RADM) and normalize the factors to the multiplication factor of the band
  # that has been used for the dark object extraction.
  normalized_radm <- coefs$RADM / coefs$RADM[bnbr]
  
  # Compute first estimate of path radiance for all band_wls based on the values
  # the band used to derive the dark object properties
  Lp_min <- coefs$RADM[bnbr] * DNmin + coefs$RADA[bnbr]
  Lp_min_band_wls <- Lp_min * scattering_factors
  # DN way
  # DNmin_basis <- DNmin - coefs$RADA[bnbr]
  # DNmin_band_wls <- DNmin_basis * scattering_factors
  # DNmin_band_wls_radm <- DNmin_band_wls * normalized_radm[1:6]
  # DNmin_band_wls_rad <- DNmin_band_wls_radm + coefs$RADA[1:6]
  # coefs$RADM[1:6] * DNmin_band_wls_rad + coefs$RADA[1:6]
  
  # Compute a correction term for the path radiance values to consider a minimum
  # surface reflection wich one can always expect.
  E0 <- ESun / earthSun(coefs$DATE)^2
  cos_szen <- cos(coefs$SUNZEN[1] * pi / 180.0)
  Tv <- 1.0
  Tz <- cos_szen
  Edown <- 0.0
  Lp_adj <- dos_adjust * (E0 * cos_szen * Tz + Edown) * Tv / pi
  
  # Compute final path radiance estimate for all band_wls
  Lp <- Lp_min_band_wls - Lp_adj
  return(Lp)
}
