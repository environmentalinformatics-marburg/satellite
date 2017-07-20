# Function used to create sysdata.rda (i.e. LUT)
#
# @description
# Function which has been used to create the LUT data of this package.
# 
# @name pck_lut
# 
NULL

pck_lut <- function(){
  # Sensor id patterns
  sensor_id_pattern <- do.call("rbind", sensor_ids_list <- list(
    data.frame(SID = "LT4",
               PATTERN = c("LT4"),
               PATTERN2 = "LT04",
               SGRP = "Landsat",
               stringsAsFactors = FALSE),
    data.frame(SID = "LT5",
               PATTERN = c("LT5"),
               PATTERN2 = "LT05",
               SGRP = "Landsat",
               stringsAsFactors = FALSE),
    data.frame(SID = "LE7",
               PATTERN = c("LE7"),
               PATTERN2 = "LE07",
               SGRP = "Landsat",
               stringsAsFactors = FALSE),
    data.frame(SID = "LC8",
               PATTERN = c("LC8"),
               PATTERN2 = "LC08",
               SGRP = "Landsat",
               stringsAsFactors = FALSE),
    data.frame(SID = "L5",
               PATTERN = c("L5"),
               PATTERN2 = "N/A",
               SGRP = "GLS",
               stringsAsFactors = FALSE),
    data.frame(SID = "MOD",
               PATTERN = c("MOD"),
               PATTERN2 = "N/A",
               SGRP = "Terra-MODIS",
               stringsAsFactors = FALSE),
    data.frame(SID = "MYD",
               PATTERN = c("MYD"),
               PATTERN2 = "N/A",
               SGRP = "Aqua-MODIS",
               stringsAsFactors = FALSE)
  ))
  
  
  # Sensor names
  sensors <- c(LT4 = "Landsat 4", LT5 = "Landsat 5", LE7 = "Landsat 7", 
               LC8 = "Landsat 8", L5 = "Global Land Survey L5",
               MOD = "Terra-MODIS", MYD = "Aqua-MODIS")
  
  # Sensor band variables
  bands <- c(LT4 = "L4_BANDS", LT5 = "L5_BANDS", LE7 = "L7_BANDS", 
             LC8 = "L8_BANDS", L5 = "GLS5_BANDS",
             MOD = "MOD_BANDS", MYD = "MYD_BANDS")
  
  # Sensor rsr
  rsr <- c(LE7 = "L7_RSR", LC8 = "L8_RSR")
  
  
  # Band wavelengths, bandwith data taken from
  # http://landsat.usgs.gov/band_designations_landsat_satellites.php
  l4_bands <- data.frame(
    SID = "LT4",
    SGRP = "Landsat",
    BID = seq(7),
    BCDE = c(sprintf("B%03dn", seq(7))),
    LMIN = c(0.45, 0.52, 0.63, 0.76, 1.55, 10.40, 2.08),
    LMAX = c(0.52, 0.60, 0.69, 0.90, 1.75, 12.50, 2.35),
    #SRES = c(30, 30, 30, 30, 30, 30, 30),
    TYPE = c("VIS", "VIS", "VIS", "NIR", "SWIR", "TIR", 
             "SWIR"),
    SPECTRUM = c("solar", "solar", "solar", "solar", "solar",
                 "thermal", "solar"))
  rownames(l4_bands) <- paste0("Band_", l4_bands$BID)
  
  l5_bands <- data.frame(
    SID = "LT5",
    SGRP = "Landsat", 
    BID = c(seq(7), "QA"),
    BCDE = c(sprintf("B%03dn", seq(7)), "B0QAn"),
    LMIN = c(0.45, 0.52, 0.63, 0.76, 1.55, 10.40, 2.08, NA),
    LMAX = c(0.52, 0.60, 0.69, 0.90, 1.75, 12.50, 2.35, NA),
    #SRES = c(30, 30, 30, 30, 30, 30, 30),
    TYPE = c("VIS", "VIS", "VIS", "NIR", "SWIR", "TIR", 
             "SWIR", "QA"),
    SPECTRUM = c("solar", "solar", "solar", "solar", "solar",
                 "thermal", "solar", NA))
  rownames(l5_bands) <- paste0("Band_", l5_bands$BID)
  
  l7_bands <- data.frame(
    SID = "LE7",
    SGRP = "Landsat",
    BID = c(seq(5), "6_VCID_1", "6_VCID_2", 7:8),
    BCDE = c(sprintf("B%03dn", seq(5)), "B0061", "B0062", sprintf("B%03dn", 7:8)),
    LMIN = c(0.45, 0.52, 0.63, 0.77, 1.55, 10.40, 10.40, 2.09, 0.52),
    LMAX = c(0.52, 0.60, 0.69, 0.90, 1.75, 12.50, 12.50, 2.35, 0.90),
    #SRES = c(30, 30, 30, 30, 30, 30, 30, 30, 15),
    TYPE = c("VIS", "VIS", "VIS", "NIR", "SWIR", "TIR", "TIR", "SWIR", "PCM"),
    SPECTRUM = c("solar", "solar", "solar", "solar", "solar",
                 "thermal", "thermal", "solar", "solar"))
  rownames(l7_bands) <- paste0("Band_", l7_bands$BID)
  
  l8_bands <- data.frame(
    SID = "LC8",
    SGRP = "Landsat",
    BID = c(seq(11), "QA"),
    BCDE = c(sprintf("B%03dn", seq(11)), "B0QAn"),
    LMIN = c(0.43, 0.45, 0.53, 0.64, 0.85, 1.57, 2.11, 0.50, 1.36, 10.60, 11.50, NA),
    LMAX = c(0.45, 0.51, 0.59, 0.67, 0.88, 1.65, 2.29, 0.68, 1.38, 11.19, 12.51, NA),
    #SRES = c(30, 30, 30, 30, 30, 30, 30, 15, 30, 30, 30, 30),
    TYPE = c("VIS", "VIS", "VIS", "VIS", "NIR", "SWIR", "SWIR", "PCM", "SWIR",
             "TIR", "TIR", "QA"),
    SPECTRUM = c("solar", "solar", "solar", "solar", "solar", "solar", "solar",
                 "solar", "solar", "thermal", "thermal", NA))
  rownames(l8_bands) <-  paste0("Band_", l8_bands$BID)
  
  gls5_bands <- data.frame(
    SID = "GLS5",
    SGRP = "Landsat",
    BID = c(seq(7), "DEM"),
    BCDE = c(sprintf("B%03dn", seq(7)), "DEM"),
    LMIN = c(0.45, 0.52, 0.63, 0.76, 1.55, 10.40, 2.08, NA),
    LMAX = c(0.52, 0.60, 0.69, 0.90, 1.75, 12.50, 2.35, NA),
    #SRES = c(30, 30, 30, 30, 30, 30, 30),
    TYPE = c("VIS", "VIS", "VIS", "NIR", "SWIR", "TIR", 
             "SWIR", "DEM"),
    SPECTRUM = c("solar", "solar", "solar", "solar", "solar",
                 "thermal", "solar", "DEM"))
  rownames(gls5_bands) <- paste0("Band_", gls5_bands$BID)
  
  mod_bands <- data.frame(
    SID = "MOD",
    SGRP = "Terra-MODIS",
    BID = c(seq(10, 120, 10), "131", "132", "141", "142", seq(150, 360, 10)),
    BCDE = c(sprintf("B%03dn", seq(10, 120, 10)), 
             "B131n", "B132n", "B141n", "B142n", 
             sprintf("B%03dn", seq(150, 360, 10))),
    LMIN = c(0.6181476, 0.8283453, 0.4542292, 0.5411443, 1.2206666, 1.6034212, 
             2.0754640, 0.4031684, 0.4351608, 0.4798235, 0.5215675, 0.5392305, 
             0.6582006, 0.6582006, 0.6684330, 0.6684330, 0.7392894, 0.8542711, 
             0.8782809, 0.9253254, 0.8983076, 3.6509025, 3.9165764, 3.9063685, 
             3.9959733, 4.3745532, 4.4521298, 1.3465556, 6.5557060, 7.0930071, 
             8.2599459, 9.4668608, 10.6066866, 11.7226057, 13.1484776, 
             13.4300604, 13.6801291, 13.9706268),
    LMAX = c(0.6751655, 0.8853492, 0.4792264, 0.5671609, 1.2626727, 1.6514183, 
             2.1604619, 0.4213848, 0.4491426, 0.4948276, 0.5389541, 0.5552039, 
             0.6741939, 0.6741939, 0.6861149, 0.6861149, 0.7543044, 0.8782920, 
             0.9292740, 0.9473220, 0.9703046, 3.9108620, 4.0466352, 4.0365319, 
             4.1259875, 4.5194578, 4.5971141, 1.4145306, 7.0057039, 7.6179643, 
             8.8299685, 9.9767981, 11.4314585, 12.3726492, 13.5886402, 
             13.9297733, 14.1699448, 14.4506626),
    #SRES = c(30, 30, 30, 30, 30, 30, 30),
    TYPE = c("VIS", "NIR", "VIS", "VIS", "NIR", "NIR",
             "NIR", "VIS", "VIS", "VIS", "VIS", "VIS", 
             "VIS", "VIS", "VIS", "VIS", "NIR", "NIR",
             "NIR", "NIR", "NIR", "SWIR", "SWIR", "SWIR", 
             "SWIR", "SWIR", "SWIR", "NIR", "TIR", "TIR", 
             "TIR", "TIR", "TIR", "TIR", "TIR", "TIR", 
             "TIR", "TIR"),
    SPECTRUM = c(rep("solar", 21), rep("thermal", 6), "solar", 
                 rep("thermal", 10)))
  rownames(mod_bands) <- paste0("Band_", mod_bands$BID)
  
  myd_bands <- data.frame(
    SID = "myd",
    SGRP = "Aqua-MODIS",
    BID = c(seq(10, 120, 10), "131", "132", "141", "142", seq(150, 360, 10)),
    BCDE = c(sprintf("B%03dn", seq(10, 120, 10)), 
             "B131n", "B132n", "B141n", "B142n", 
             sprintf("B%03dn", seq(150, 360, 10))),
    LMIN = c(0.6181476, 0.8283453, 0.4542292, 0.5411443, 1.2206666, 1.6034212, 
             2.0754640, 0.4031684, 0.4351608, 0.4798235, 0.5215675, 0.5392305, 
             0.6582006, 0.6582006, 0.6684330, 0.6684330, 0.7392894, 0.8542711, 
             0.8782809, 0.9253254, 0.8983076, 3.6509025, 3.9165764, 3.9063685, 
             3.9959733, 4.3745532, 4.4521298, 1.3465556, 6.5557060, 7.0930071, 
             8.2599459, 9.4668608, 10.6066866, 11.7226057, 13.1484776, 
             13.4300604, 13.6801291, 13.9706268),
    LMAX = c(0.6751655, 0.8853492, 0.4792264, 0.5671609, 1.2626727, 1.6514183, 
             2.1604619, 0.4213848, 0.4491426, 0.4948276, 0.5389541, 0.5552039, 
             0.6741939, 0.6741939, 0.6861149, 0.6861149, 0.7543044, 0.8782920, 
             0.9292740, 0.9473220, 0.9703046, 3.9108620, 4.0466352, 4.0365319, 
             4.1259875, 4.5194578, 4.5971141, 1.4145306, 7.0057039, 7.6179643, 
             8.8299685, 9.9767981, 11.4314585, 12.3726492, 13.5886402, 
             13.9297733, 14.1699448, 14.4506626),
    #SRES = c(30, 30, 30, 30, 30, 30, 30),
    TYPE = c("VIS", "NIR", "VIS", "VIS", "NIR", "NIR",
             "NIR", "VIS", "VIS", "VIS", "VIS", "VIS", 
             "VIS", "VIS", "VIS", "VIS", "NIR", "NIR",
             "NIR", "NIR", "NIR", "SWIR", "SWIR", "SWIR", 
             "SWIR", "SWIR", "SWIR", "NIR", "TIR", "TIR", 
             "TIR", "TIR", "TIR", "TIR", "TIR", "TIR", 
             "TIR", "TIR"),
    SPECTRUM = c(rep("solar", 21), rep("thermal", 6), "solar", 
                 rep("thermal", 10)))
  rownames(myd_bands) <- paste0("Band_", myd_bands$BID)
  
  # Landat 7 relative spectral response (units: nm-1)
  l7_rsr <- readRDS(system.file("extdata", "l7_rsr.rds", package = "satellite"))
  l7_rsr$RSR <- as.numeric(as.character(l7_rsr$RSR))
  
  # Landat 8 relative spectral response (units: nm-1)
  l8_rsr <- readRDS(system.file("extdata", "l8_rsr.rds", package = "satellite"))
  l8_rsr$RSR <- as.numeric(as.character(l8_rsr$RSR))
  
  # Solar irradiance (units: W m-2 nm-1)
  solar <- readRDS(system.file("extdata", "solar.rds", package = "satellite"))
  for(i in seq(2,8)){
    solar[, i] <- as.numeric(as.character(solar[, i]))
  }
  
  # Tabulated values of ESun (W m-2 micrometer-1)
  l4_esun <- c(1957, 1826, 1554, 1036, 215, NA, 80.67)
  attr(l4_esun, "names") <- as.character(l4_bands$BCDE)
  
  l5_esun <- c(1957, 1825, 1557, 1033, 214.9, NA, 80.72)
  attr(l5_esun, "names") <- as.character(l5_bands$BCDE)
  
  l7_esun <- c(1997, 1812, 1533, 1039, 230.8, NA, NA, 84.90, 1362)
  attr(l7_esun, "names") <- as.character(l7_bands$BCDE)

  gls5_esun <- c(1957, 1825, 1557, 1033, 214.9, NA, 80.72, NA)
  attr(gls5_esun, "names") <- as.character(gls5_bands$BCDE)
  
  mod_esun <- c(1606.17, 992.2, 2087.94, 1865.94, 474.34, 240.23, 90.33, 
                1745.75, 1903.77, 1980.94, 1884.19, 1892.24, 1548.18, 
                1548.18, 1508.14, 1508.14, 1294.8, 973, 934.8, 873.7, 
                873.2, NA, NA, NA, NA, NA, NA, 364.95, NA, NA, NA, NA, 
                NA, NA, NA, NA, NA, NA)
  attr(mod_esun, "names") <- as.character(mod_bands$BCDE)
  
  myd_esun <- c(1608.05, 991.33, 2088.17, 1865.27, 474.94, 240.61, 90.4, 
                1747.74, 1906.19, 1977.14, 1885.26, 1892.84, 1547.47, 
                1547.47, 1506.12, 1506.12, 1294.69, 973.21, 934.5, 872.39, 
                873.11, NA, NA, NA, NA, NA, NA, 365.07, NA, NA, NA, NA, 
                NA, NA, NA, NA, NA, NA)
  attr(myd_esun, "names") <- as.character(myd_bands$BCDE)
  
  meta <- list(SENSORS = "Sensor ids and names",
               SENSOR_ID_PATTERN = "Filename patter of sensor",
               BANDS = "Sensor ids and meta names for band information",
               RSR = "RSR ids and sensor names",
               L4_BANDS = "Band information for Landsat 4 bands",
               L5_BANDS = "Band information for Landsat 5 bands",
               L7_BANDS = "Band information  for Landsat 7 bands",
               L8_BANDS = "Band information  for Landsat 8 bands",
               GLS5_BANDS = "Band information  for GLS with Landsat 5",
               MOD_BANDS = "Band information  for Terra-MODIS bands",
               MYD_BANDS = "Band information  for Aqua-MODIS bands",
               L7_SRS = "Landat 7 relative spectral response (nm-1) taken from http://landsat.usgs.gov/instructions.php",
               L8_SRS = "Landat 8 relative spectral response (nm-1) taken from http://landsat.usgs.gov/instructions.php",
               SOLAR = "Solar irradiance (units: W m-2 nm-1) from the National Renewable Energy Laboratory taken from http://rredc.nrel.gov/solar/spectra/am0/modtran.html",
               L5_ESUN = "Tabulated ESun values from Chander and Markham (2003), tab. II, taken from http://landsathandbook.gsfc.nasa.gov/pdfs/L5TMLUTIEEE2003.pdf",
               L4_ESUN = "Tabulated ESun values from Chander and Markham (2003), tab. II, taken from http://landsathandbook.gsfc.nasa.gov/pdfs/L5TMLUTIEEE2003.pdf",
               L7_ESUN = "Tabulated ESun values from Landsat7 handbook, tab 11.3 (Thuillier SPECTRUM), taken from http://landsathandbook.gsfc.nasa.gov/pdfs/Landsat7_Handbook.pdf",
               GLS5_ESUN = "Tabulated ESun values from Chander and Markham (2003), tab. II, taken from http://landsathandbook.gsfc.nasa.gov/pdfs/L5TMLUTIEEE2003.pdf",
               MOD_ESUN = "Tabulated ESun values from http://mcst.gsfc.nasa.gov/calibration/parameters",
               MYD_ESUN = "Tabulated ESun values from http://mcst.gsfc.nasa.gov/calibration/parameters")
  
  # Create sysdata.rda
  lut <- list(SENSORS = sensors,
              SENSOR_ID_PATTERN = sensor_id_pattern,
              BANDS = bands,
              RSR = rsr,
              L4_BANDS = l4_bands, L5_BANDS = l5_bands, 
              L7_BANDS = l7_bands, L8_BANDS = l8_bands,
              GLS5_BANDS = gls5_bands,
              MOD_BANDS = mod_bands, MYD_BANDS = myd_bands,
              L7_RSR = l7_rsr, L8_RSR = l8_rsr, SOLAR = solar, 
              L4_ESUN = l4_esun, L5_ESUN = l5_esun, L7_ESUN = l7_esun,
              GLS5_ESUN = gls5_esun, 
              MOD_ESUN = mod_esun, MYD_ESUN = myd_esun,
              META = meta)
  
  devtools::use_data(lut, overwrite = TRUE, internal = TRUE)
}
