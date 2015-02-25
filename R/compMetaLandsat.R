#' Get calibration information from Landsat 8 standard level 1B/T filename.
#'
#' @description
#' The function scans a Lansat metadata file for various calibration 
#' and orbit coefficients as well as some sensor specific data.
#'
#' @param filepath path and filename to the landsat metadata file
#'
#' @return Dataframe containing the following information for each band
#' \itemize{
#'   \item Date (DATE), sensor (SENSOR) and band number (BNBR)
#'   \item addtition and multiplication coefficients for reflectance 
#'   (REFA, REFM) and radiance (RADA, RADM)
#'   \item brightness temperature correction parameters (BTK1, BTK2)
#'   \item sun elevation angle (SELEV), sun zenith angle (SZEN) and sun azimuth 
#'   angle (SAZM).
#'   \item Radiance/reflectance maximum and minimum values (RADMAX, RADMIN, 
#'   REFMAX, REFMIN)
#'   \item Filepath (FILE) of the sensor data and metadata file (METAFILE)
#' }
#' 
#' @export compMetaLandsat
#'
#' @examples
#' path <- system.file("extdata", package = "satellite")
#' files <- list.files(path, pattern = glob2rx("LC8*.tif"), full.names = TRUE)
#' compMetaLandsat(files)
#' 
compMetaLandsat <- function(files){
  
  datafiles <- compFilePathLandsat(files)
  
  bandinfo <- lutInfoBandsFromSID(datafiles$SID[1])
  bandinfo <- merge(bandinfo, datafiles, by = "BCDE")
  
  metadata <- read.table(as.character(bandinfo$METAFILE[1]), header = FALSE, 
                         sep = "=", fill = TRUE)
  
  search_term_date <- "DATE_ACQUIRED"
  search_term_esd <- "EARTH_SUN_DISTANCE"
  
  metainformation <- lapply(seq(nrow(bandinfo)), function(x){
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
    
    date <- strftime(as.character(
      (subset(metadata$V2, gsub("\\s","", metadata$V1) == search_term_date))))
    esd <- as.numeric(as.character(
      (subset(metadata$V2, gsub("\\s","", metadata$V1) == search_term_esd))))
    selv <- as.numeric(as.character(
      subset(metadata$V2, gsub("\\s","", metadata$V1) == "SUN_ELEVATION")))
    sazm <- as.numeric(as.character(
      subset(metadata$V2, gsub("\\s","", metadata$V1) == "SUN_AZIMUTH")))
    szen <- 90.0 - selv

    if(length(cal_rad_max) == 0){cal_rad_max = NA}
    if(length(cal_rad_min) == 0){cal_rad_min = NA}
    if(length(cal_ref_max) == 0){cal_ref_max = NA}
    if(length(cal_ref_min) == 0){cal_ref_min = NA}
    if(length(cal_add_rad) == 0){cal_add_rad = NA}
    if(length(cal_mult_rad) == 0){cal_mult_rad = NA}
    if(length(cal_add_ref) == 0){cal_add_ref = NA}
    if(length(cal_mult_ref) == 0){cal_mult_ref = NA}
    if(length(cal_BTK1) == 0){cal_BTK1 = NA}
    if(length(cal_BTK2) == 0){cal_BTK2 = NA}
    if(length(esd) == 0){esd = NA}
    
    result <- data.frame(DATE = date, 
                         SID = bandinfo$SID[x],
                         SENSOR = bandinfo$SENSOR[x],
                         BIDS = bandinfo$BIDS.x[x],
                         BCDE = bandinfo$BCDE[x],
                         SRES = bandinfo$SRES[x],
                         TYPE = bandinfo$TYPE[x],
                         SPECTRUM = bandinfo$SPECTRUM[x],
                         RADA = cal_add_rad,
                         RADM = cal_mult_rad,
                         REFA = cal_add_ref,
                         REFM = cal_mult_ref,
                         BTK1 = cal_BTK1,
                         BTK2 = cal_BTK2,
                         SUNZEN = szen,
                         SUNAZM = sazm,
                         SUNELEV = selv,
                         ESD = esd,
                         RADMAX = cal_rad_max,
                         RADMIN = cal_rad_min,
                         REFMAX = cal_ref_max,
                         REFMIN = cal_ref_min,
                         FILE = bandinfo$FILE[x],
                         FILE = bandinfo$METAFILE[x])
  })
  metainformation <- do.call("rbind", metainformation)
  return(metainformation)
}