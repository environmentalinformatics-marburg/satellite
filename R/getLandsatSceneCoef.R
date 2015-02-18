#' Get calibration information from Landsat 8 standard level 1B/T filename.
#'
#' @description
#' The function scans a Lansat 8 metadata file for calibration coefficients and 
#' solar geometry information. Depending on the user selection, the calibration
#' coefficients are for reflectance or radiance.
#'
#' @param filepath path and filename to the landsat metadata file
#' @param band band number for which the coefficients should be retrieved
#' @param coef either "rad" or "ref" for radiance/reflectance coefficients
#'
#' @return vector containing, multiplication coefficient [1], 
#' addition coefficient [2], sun elevation angle [3], sun zenith angle [4], 
#' sun azimuth angle [5]
#'
#' @export getSceneCoef
#'
#' @examples
#' not run:
#' getSceneCoef(filepath = "Name_of_Landsat_Metadata_File", band = 5)

getSceneCoef <- function(filepath, band, coef = "rad"){
  cal.data <- read.table(filepath, header = FALSE, sep = "=", fill = TRUE)
  
  if(coef == "ref"){
    search.term.mult <- paste0("REFLECTANCE_MULT_BAND_", band)
    search.term.add <- paste0("REFLECTANCE_ADD_BAND_", band)
  } else {
    search.term.mult <- paste0("RADIANCE_MULT_BAND_", band)
    search.term.add <- paste0("RADIANCE_ADD_BAND_", band)
  }
  cal.mult <- as.numeric(as.character(
    (subset(cal.data$V2, gsub("\\s","", cal.data$V1) == search.term.mult))))
  cal.add <- as.numeric(as.character(
    (subset(cal.data$V2, gsub("\\s","", cal.data$V1) == search.term.add))))
  if(length(cal.mult) == 0){
    cal.mult = 1.0
  }
  if(length(cal.add) == 0){
    cal.add = 0.0
  }
  
  selv <- as.numeric(as.character(
    subset(cal.data$V2, gsub("\\s","", cal.data$V1) == "SUN_ELEVATION")))
  sazm <- as.numeric(as.character(
    subset(cal.data$V2, gsub("\\s","", cal.data$V1) == "SUN_AZIMUTH")))
  szen <- 90.0 - selv
  result <- c(cal.mult, cal.add, selv, szen, sazm)
  attr(result, "Info") <- c("CalMult", "CalAdd", "SunElev", "SunZen", "SunAzm")
  return(result)
}