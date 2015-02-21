#' Compute path radiance based on dark object method
#'
#' @description
#' Compute an estimaed path radiance for all sensor bands using a dark object 
#' method which can be used to roughly correct the radiance values for 
#' atmospheric scattering.
#'
#' @param red a raster of the sensor's red band
#' @param nir a raster of the sensor's nir band
#' @param swir a raster of the sensor's swir band
#'
#' @return raster object with invariant pixels marked with 1, 0 otherwise
#'
#' @export pathRadianceDOS
#' 
#' @details The dark object substraction approach is based on an approximation 
#' of the atmospheric path radiance (i.e. upwelling radiation which is 
#' scatter into the sensors field of view, aka haze) using the reflectance of a 
#' dark object (i.e. reflectance ~1%). 
#' 
#' Chavez Jr PS (1988) proposed a method which uses the dark object reflectance
#' in one band ot predict the corresponding path radiances in all other bands 
#' using a relative radiance model which depends on the wavlelength and overall
#' atmospheric optical thickness (which is estimated based on the dark object's
#' DN value). This has the advantage that the path radiance is actually 
#' correlated across different sensor bands and not computed individuall for 
#' each band using independent dark objects.
#' 
#' The relative radiance model follows a wavelength dependent scattering which
#' ranges from -4 power over -2, -1, -0.7 to -0.5 power for very clear over
#' clear, moderate, hazy to very hazy conditions. The relative factors are
#' computed individually for each 1/1000 wavelength within each band range
#' and subsequently averaged over the band.
#' 
#' @references This function is an extended version of the DOS function from 
#' Sarah C. Goslee (2011) Analyzing Remote Sensing Data in R: The landsat 
#' Package. Journal of Statistical Software,43/4, 1-25. URL 
#' \url{http://www.jstatsoft.org/v43/i04/}.
#' 
#' The underlaying theory has been published by Chavez Jr PS (1988) An improved 
#' dark-object subtraction technique for atmospheric scattering correction of 
#' multispectral data. Remote Sensing of Environment 24/3, 
#' doi:10.1016/0034-4257(88)90019-3, available online at
#'  \url{http://www.sciencedirect.com/science/article/pii/0034425788900193}.
#'
#' If you refer to Sawyer and Stephen 2014, please note that eq. 5 is wrong.
#' 
#' @examples
#' not run:
#' pathRadianceDOS(red, nir, swir, level=.99)

pathRadianceDOS <- function(sensor = "Landsat 8", DNmin = 69, bnbr = 1, coefs, date, 
                                   scat_coefs = c(-4.0, -2.0, -1.0, -0.7, -0.5),
                                   dos_adjust = 0.01){
  
  # Define band wavelengths and solar constant. Bandwith data taken from
  # http://landsat.usgs.gov/band_designations_landsat_satellites.php
  if(sensor == "Landsat 4" | sensor == "Landsat 5"){
    bands <- data.frame(
      lmin <- c(0.45, 0.52, 0.63, 0.76, 1.55, 10.40, 2.08),
      lmax <- c(0.52, 0.60, 0.69, 0.90, 1.75, 12.50, 2.35))
    rownames(bands) <- seq(7)
    Esun <- c(198.3, 179.6, 153.6, 103.1, 22, 8.34)
  } else if(sensor == "Landsat 7"){
    bands <- data.frame(
      lmin <- c(0.45, 0.52, 0.63, 0.77, 1.55, 10.40, 2.09, 0.52),
      lmax <- c(0.52, 0.60, 0.69, 0.90, 1.75, 12.50, 2.35, 0.90))
    rownames(bands) <- seq(8)
    Esun <- c(1970, 1842, 1547, 1044, 225.7, 0.00, 82.06, 1396)
  } else if(sensor == "Landsat 8"){
    bands <- data.frame(
      lmin <- c(0.43, 0.45, 0.53, 0.64, 0.85, 1.57, 2.11, 0.50, 1.36, 10.60, 
                11.50),
      lmax <- c(0.45, 0.51, 0.59, 0.67, 0.88, 1.65, 2.29, 0.68, 1.38, 11.19, 
                12.51))
    rownames(bands) <- seq(11)
    Esun <- c(0.0, 1970, 1842, 1547, 1044, 225.7, 225.7, 1396, 225.7, 0.00, 0.00)
  } else {
    stop("The satellite you have provided is not implemented.")  
  }
  
  # Define relative scattering model based on wavelength dependent scattering
  # processes and different atmospheric optical thiknesses. Resulting 
  # coefficients are the band wavelength to the power of the respective 
  # scattering coefficients defined for the model.
  scattering_model <- matrix(NA, nrow = nrow(bands), ncol = length(scat_coefs))
  rownames(scattering_model) <- rownames(bands)
  colnames(scattering_model) <- paste0("coef", scat_coefs)
  for(i in 1:nrow(bands)) {
    act_band <- seq(bands[i, 1], bands[i, 2], by=0.001)
    for(j in 1:length(scat_coefs)){
      scattering_model[i, j] <- mean(act_band ^ scat_coefs[j])
    }
  }
  
  # Compute multiplication factors which relate the scattering model coefficents
  # between the individual bands and the one band which has been used for the
  # extraction of the dark object radiation.
  basline_factor <- scattering_model[bnbr, ]
  scattering_factors <- sweep(scattering_model, 2, basline_factor, "/")
  
  # Compute radiation multiplication factors for radiance conversion (i.e.
  # RADM) and normalize the factors to the multiplication factor of the band
  # that has been used for the dark object extraction.
  normalized_radm <- coefs$RADM / coefs$RADM[bnbr]
  
  # Compute first estimate of path radiance for all bands based on the values of
  # the band used to derive the dark object properties
  Lp_min <- coefs$RADM[bnbr] * DNmin + coefs$RADA[bnbr]
  Lp_min_bands <- Lp_min * scattering_factors
  # DN way
  # DNmin_basis <- DNmin - coefs$RADA[bnbr]
  # DNmin_bands <- DNmin_basis * scattering_factors
  # DNmin_bands_radm <- DNmin_bands * normalized_radm[1:6]
  # DNmin_bands_rad <- DNmin_bands_radm + coefs$RADA[1:6]
  # coefs$RADM[1:6] * DNmin_bands_rad + coefs$RADA[1:6]
  
  # Compute a correction term for the path radiance values to consider a minimum
  # surface reflection wich one can always expect.
  E0 <- Esun[bnbr] / ESdist(date)^2
  cos_szen <- cos(coefs$SUNZEN[1] * pi / 180.0)
  Tv <- 1.0
  Tz <- cos_szen
  Edown <- 0.0
  Lp_adj <- dos_adjust * (E0 * cos_szen * Tz + Edown) * Tv / pi
  
  # Compute final path radiance estimate for all bands
  Lp <- Lp_min_bands - Lp_adj
}
