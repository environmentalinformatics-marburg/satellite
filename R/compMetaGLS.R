#' Get calibration information from GLS standard level 1B/T filename
#'
#' @description
#' The function scans a Global Land Survey (GLS) metadata file for various calibration 
#' and orbit coefficients as well as some sensor specific data.
#'
#' @param files Path and filename of the GLS metadata file. 
#'
#' @return \code{data.frame} containing the following information for each 
#' band/layer: 
#' \itemize{
#'   \item DATE date (e.g. 2013-07-07)
#'   \item SID sensor id (e.g. LC8)
#'   \item SENSOR sensor name (e.g. GLS5)
#'   \item SGRP sensor group (e.g. Landsat)
#'   \item BID band id (e.g. 7)
#'   \item BCDE band code (5 digit standard name, e.g B001n)
#'   \item SRES spatial resolution of the sensor band (e.g. 30 for 30 m x 30m)
#'   \item TYPE type of the sensor band regarding wavelength (e.g. VIS)
#'   \item SPECTRUM spectral range regarding radiation source (e.g. solar)
#'   \item CALIB type of applied calibration (e.g. SC for scaled counts)
#'   \item RID region id (e.g. R00001) for multi region satellite objects
#'   \item RADA addtition coefficient for radiance conversion
#'   \item RADM multiplication coefficient for radiance conversion
#'   \item REFA addtition coefficient for reflectance conversion
#'   \item REFM multiplication coefficient for reflectance conversion
#'   \item BTK1 brightness temperature correction parameter
#'   \item BTK2 brightness temperature correction parameter
#'   \item SZEN sun zenith angle
#'   \item SAZM sun azimuth angle
#'   \item SELV sun elevation angle
#'   \item ESD earth-sun distance (AU)
#'   \item LMIN Minimum wavelength of the band (micrometer)
#'   \item LMAX Maximum wavelength of the band (micrometer)
#'   \item RADMIN Minimum radiance recorded by the band
#'   \item RADMAX Maximum radiance recorded by the band
#'   \item REFMIN Minimum reflectance recorded by the band
#'   \item REFMAX Maximum reflectance recorded by the band
#'   \item LNBR Layer number from 1 to n layers
#'   \item LAYER Layer name
#'   \item FILE Filepath of the data file
#'   \item METAFILE Filepath of the metadata file
#' }
#' 
#' @export compMetaGLS
#'
#' @examples
#' path <- system.file("extdata", package = "satellite")
#' files <- list.files(path, pattern = glob2rx("LC8*.TIF"), full.names = TRUE)
#' compMetaGLS(files)
#' 
compMetaGLS <- function(files){
  
  datafiles <- compFilePathGLS(files)
  
  bandinfo_all <- lutInfoBandsFromSID(datafiles$SID[1])
  bandinfo_all <- merge(bandinfo_all, datafiles, by = "BCDE")
  
  metainformation <- lapply(seq(length(sort(unique(bandinfo_all$META)))), function(scn){
    bandinfo <- bandinfo_all[bandinfo_all$META == sort(unique(bandinfo_all$META))[scn], ]
    metadata <- utils::read.table(as.character(bandinfo$METAFILE[1]), header = FALSE, 
                                  sep = "=", fill = TRUE)
    
    search_term_date <- "ACQUISITION_DATE"
    search_term_esd <- "EARTH_SUN_DISTANCE"
    
    metainformation <- lapply(seq(nrow(bandinfo)), function(x){
      search_term_L_max <- paste0("LMAX_BAND", x)
      search_term_L_min <- paste0("LMIN_BAND", x)
      search_term_Q_max <- paste0("QCALMAX_BAND", x)
      search_term_Q_min <- paste0("QCALMIN_BAND", x)
      
      cal_L_max <- as.numeric(as.character(
        (subset(metadata$V2, gsub("\\s","", metadata$V1) == search_term_L_max))))
      cal_L_min <- as.numeric(as.character(
        (subset(metadata$V2, gsub("\\s","", metadata$V1) == search_term_L_min))))
      cal_Q_max <- as.numeric(as.character(
        (subset(metadata$V2, gsub("\\s","", metadata$V1) == search_term_Q_max))))
      cal_Q_min <- as.numeric(as.character(
        (subset(metadata$V2, gsub("\\s","", metadata$V1) == search_term_Q_min))))
      date <- as.POSIXlt(as.character(
        (subset(metadata$V2, gsub("\\s","", metadata$V1) == search_term_date))), 
        tz = "UTC")

      esd <- as.numeric(as.character(
        (subset(metadata$V2, gsub("\\s","", metadata$V1) == search_term_esd))))
      selv <- as.numeric(as.character(
        subset(metadata$V2, gsub("\\s","", metadata$V1) == "SUN_ELEVATION")))
      sazm <- as.numeric(as.character(
        subset(metadata$V2, gsub("\\s","", metadata$V1) == "SUN_AZIMUTH")))
      szen <- 90.0 - selv
      
      if(length(cal_L_max) == 0){cal_L_max = NA}
      if(length(cal_L_min) == 0){cal_L_min = NA}
      if(length(cal_Q_max) == 0){cal_Q_max = NA}
      if(length(cal_Q_min) == 0){cal_Q_min = NA}
      if(length(esd) == 0){esd = NA}
      
      cal_mult_rad <- ((cal_L_max - cal_L_min)/(cal_Q_max-cal_Q_min))
      cal_add_rad<- (-1 * cal_mult_rad) * cal_Q_min + cal_L_min
      
      result <- data.frame(SCENE = scn,
                           DATE = date, 
                           SID = bandinfo$SID.x[x],
                           SENSOR = bandinfo$SENSOR[x],
                           SGRP = bandinfo$SGRP[x],
                           BID = bandinfo$BID.x[x],
                           BCDE = bandinfo$BCDE[x],
                           #SRES = bandinfo$SRES[x],
                           TYPE = bandinfo$TYPE[x],
                           SPECTRUM = bandinfo$SPECTRUM[x],
                           CALIB = bandinfo$CALIB[x],
                           RID = "R00001",
                           RADA = cal_add_rad,
                           RADM = cal_mult_rad,
                           REFA = NA,
                           REFM = NA,
                           BTK1 = NA,
                           BTK2 = NA,
                           SZEN = szen,
                           SAZM = sazm,
                           SELV = selv,
                           ESD = esd,
                           LMIN = bandinfo$LMIN[x],
                           LMAX = bandinfo$LMAX[x],
                           RADMAX = cal_L_max,
                           RADMIN = cal_L_min,
                           REFMAX = NA,
                           REFMIN = NA,
                           LNBR = x,
                           LAYER = bandinfo$LAYER[x],
                           FILE = bandinfo$FILE[x],
                           METAFILE = bandinfo$METAFILE[x],
                           stringsAsFactors = FALSE)
    })
    metainformation <- do.call("rbind", metainformation)
    return(metainformation)
  })
  return(do.call("rbind", metainformation))
}