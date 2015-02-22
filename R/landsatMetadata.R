#' Get calibration information from Landsat 8 standard level 1B/T filename.
#'
#' @description
#' The function scans a Lansat 8 metadata file for calibration coefficients and 
#' solar geometry information. Depending on the user selection, the calibration
#' coefficients are for reflectance or radiance.
#'
#' @param filepath path and filename to the landsat metadata file
#'
#' @return Dataframe containing addtition and multiplication coefficients for 
#' reflectance (REFA, REFM) and radiance (RADA, RADM), brightness temperature
#' correction parameters (BTK1, BTK2), sun elevation angle (SELEV), 
#' sun zenith angle (SZEN) and sun azimuth angle (SAZM).
#'
#' @export landsatMetadata
#'
#' @examples
#' landsat8_metadatafile <-   system.file("extdata", 
#' "LC81950252013188LGN00_MTL.txt", package = "satellite")
#' landsatMetadata(landsat8_metadatafile)
#' 
landsatMetadata <- function(filepath){
  
  metadata <- read.table(filepath, header = FALSE, sep = "=", fill = TRUE)
  
  search_term_date <- "DATE_ACQUIRED"
  search_term_earthSun <- "EARTH_SUN_DISTANCE"
  
  metainformation <- lapply(seq(1:11), function(x){
    search_term_rad_max <- paste0("RADIANCE_MAXIMUM_BAND_", x)
    search_term_rad_min <- paste0("RADIANCE_MINIMUM_BAND_", x)
    search_term_ref_max <- paste0("REFLECTANCE_MAXIMUM_BAND_", x)
    search_term_ref_min <- paste0("REFLECTANCE_MINIMUM_BAND_", x)
    
    search_term_add_rad <- paste0("RADIANCE_ADD_BAND_", x)
    search_term_mult_rad <- paste0("RADIANCE_MULT_BAND_", x)
    search_term_add_ref <- paste0("REFLECTANCE_ADD_BAND_", x)
    search_term_mult_ref <- paste0("REFLECTANCE_MULT_BAND_", x)
    search_term_BTK1 <- paste0("K1_CONSTANT_BAND_", x)
    search_term_BTK2 <- paste0("K2_CONSTANT_BAND_", x)
    
    cal_rad_max <- as.numeric(as.character(
      (subset(metadata$V2, gsub("\\s","", metadata$V1) == search_term_rad_max))))
    cal_rad_min <- as.numeric(as.character(
      (subset(metadata$V2, gsub("\\s","", metadata$V1) == search_term_rad_min))))
    cal_ref_max <- as.numeric(as.character(
      (subset(metadata$V2, gsub("\\s","", metadata$V1) == search_term_ref_max))))
    cal_ref_min <- as.numeric(as.character(
      (subset(metadata$V2, gsub("\\s","", metadata$V1) == search_term_ref_min))))
    
    cal_add_rad <- as.numeric(as.character(
      (subset(metadata$V2, gsub("\\s","", metadata$V1) == search_term_add_rad))))
    cal_mult_rad <- as.numeric(as.character(
      (subset(metadata$V2, gsub("\\s","", metadata$V1) == search_term_mult_rad))))
    cal_add_ref <- as.numeric(as.character(
      (subset(metadata$V2, gsub("\\s","", metadata$V1) == search_term_add_ref))))
    cal_mult_ref <- as.numeric(as.character(
      (subset(metadata$V2, gsub("\\s","", metadata$V1) == search_term_mult_ref))))
    cal_BTK1 <- as.numeric(as.character(
      (subset(metadata$V2, gsub("\\s","", metadata$V1) == search_term_BTK1))))
    cal_BTK2 <- as.numeric(as.character(
      (subset(metadata$V2, gsub("\\s","", metadata$V1) == search_term_BTK2))))
    
    solar = TRUE
    if(length(cal_add_ref) == 0){
      cal_ref_max = NA
      cal_ref_min = NA
      cal_add_ref = NA
      cal_mult_ref = NA
      solar = FALSE
    }
    if(solar == TRUE){
      cal_BTK1 = NA
      cal_BTK2 = NA
    }

    date <- strftime(as.character(
      (subset(metadata$V2, gsub("\\s","", metadata$V1) == search_term_date))))
    earthSun <- as.numeric(as.character(
      (subset(metadata$V2, gsub("\\s","", metadata$V1) == search_term_earthSun))))
    selv <- as.numeric(as.character(
      subset(metadata$V2, gsub("\\s","", metadata$V1) == "SUN_ELEVATION")))
    sazm <- as.numeric(as.character(
      subset(metadata$V2, gsub("\\s","", metadata$V1) == "SUN_AZIMUTH")))
    szen <- 90.0 - selv
    result <- data.frame(DATE = date, 
                         BAND = x,
                         SOLAR = solar,
                         RADA = cal_add_rad,
                         RADM = cal_mult_rad,
                         REFA = cal_add_ref,
                         REFM = cal_mult_ref,
                         BTK1 = cal_BTK1,
                         BTK2 = cal_BTK2,
                         SUNZEN = szen,
                         SUNAZM = sazm,
                         SUNELEV = selv,
                         EARTHSUN = earthSun,
                         RADMAX = cal_rad_max,
                         RADMIN = cal_rad_min,
                         REFMAX = cal_ref_max,
                         REFMIN = cal_ref_min)
  })
  metainformation <- do.call("rbind", metainformation)
  return(metainformation)
}