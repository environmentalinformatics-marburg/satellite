# Create sysdata.rda

# Sensor names
sensors <- c(LE4 = "Landsat 4", LE5 = "Landsat 5", LE7 = "Landsat 7", 
             LC8 = "Landsat 8")

# Sensor band variables
bands <- c(LE4 = "L4_BANDS", LE5 = "L5_BANDS", LE7 = "L7_BANDS", 
           LC8 = "L8_BANDS")

# Sensor rsr
rsr <- c(LE7 = "L7_RSR", LC8 = "L8_RSR")


# Band wavelengths, bandwith data taken from
# http://landsat.usgs.gov/band_designations_landsat_satellites.php
l4_bands <- data.frame(
  BIDS = seq(7),
  BCDE = c(sprintf("%03dn", seq(7))),
  LMIN = c(0.45, 0.52, 0.63, 0.76, 1.55, 10.40, 2.08),
  LMAX = c(0.52, 0.60, 0.69, 0.90, 1.75, 12.50, 2.35),
  SRES = c(30, 30, 30, 30, 30, 30, 30),
  TYPE = c("VIS", "VIS", "VIS", "NIR", "SWIR", "TIR", 
           "SWIR"),
  SPECTRUM = c("solar", "solar", "solar", "solar", "solar",
               "thermal", "solar"))
rownames(l4_bands) <- paste0("Band_", l4_bands$BIDS)

l5_bands <- data.frame(
  BIDS = seq(7),
  BCDE = c(sprintf("%03dn", seq(7))),
  LMIN = c(0.45, 0.52, 0.63, 0.76, 1.55, 10.40, 2.08),
  LMAX = c(0.52, 0.60, 0.69, 0.90, 1.75, 12.50, 2.35),
  SRES = c(30, 30, 30, 30, 30, 30, 30),
  TYPE = c("VIS", "VIS", "VIS", "NIR", "SWIR", "TIR", 
           "SWIR"),
  SPECTRUM = c("solar", "solar", "solar", "solar", "solar",
               "thermal", "solar"))
rownames(l5_bands) <- paste0("Band_", l5_bands$BIDS)

l7_bands <- data.frame(
  BIDS = c(seq(5), "6_VCID_1", "6_VCID_2", 7:8),
  BCDE = c(sprintf("%03dn", seq(5)), "0061", "0062", sprintf("%03dn", 7:8)),
  LMIN = c(0.45, 0.52, 0.63, 0.77, 1.55, 10.40, 10.40, 2.09, 0.52),
  LMAX = c(0.52, 0.60, 0.69, 0.90, 1.75, 12.50, 12.50, 2.35, 0.90),
  SRES = c(30, 30, 30, 30, 30, 30, 30, 30, 15),
  TYPE = c("VIS", "VIS", "VIS", "NIR", "SWIR", "TIR", "TIR", "SWIR", "PCM"),
  SPECTRUM = c("solar", "solar", "solar", "solar", "solar",
               "thermal", "thermal", "solar", "solar"))
rownames(l7_bands) <- paste0("Band_", l7_bands$BIDS)

l8_bands <- data.frame(
  BIDS = c(seq(11), "QA"),
  BCDE = c(sprintf("%03dn", seq(11)), "QA"),
  LMIN = c(0.43, 0.45, 0.53, 0.64, 0.85, 1.57, 2.11, 0.50, 1.36, 10.60, 11.50, NA),
  LMAX = c(0.45, 0.51, 0.59, 0.67, 0.88, 1.65, 2.29, 0.68, 1.38, 11.19, 12.51, NA),
  SRES = c(30, 30, 30, 30, 30, 30, 30, 15, 30, 30, 30, 30),
  TYPE = c("VIS", "VIS", "VIS", "VIS", "NIR", "SWIR", "SWIR", "PCM", "SWIR",
           "TIR", "TIR", "QA"),
  SPECTRUM = c("solar", "solar", "solar", "solar", "solar", "solar", "solar",
               "solar", "solar", "thermal", "thermal", NA))
rownames(l8_bands) <-  paste0("Band_", l8_bands$BIDS)

# Landat 7 relative spectral response (units: nm-1)
l7_rsr <- read.table("data-raw/landsat_7_relative_spectral_response.csv",
                     header = TRUE, sep = ";")

# Landat 8 relative spectral response (units: nm-1)
l8_rsr <- read.table("data-raw/landsat_8_relative_spectral_response.csv",
                     header = TRUE, sep = ";")

# Solar irradiance (units: W m-2 nm-1)
solar <- read.table("data-raw/mod_etr_solar_irradiance.csv",
                  header = TRUE, sep = ";")

# Tabulated values of ESun (W m-2 micrometer-1)
l4_esun <- c(1957, 1826, 1554, 1036, 215, NA, 80.67)
names(l4_esun) <- rownames(l4_bands$BIDS)

l5_esun <- c(1957, 1825, 1557, 1033, 214.9, NA, 80.72)
names(l5_esun) <- rownames(l5_bands$BIDS)

l7_esun <- c(1997, 1812, 1533, 1039, 230.8, NA, NA, 84.90, 1362)
names(l7_esun) <- rownames(l7_bands$BIDS)

meta <- list(SENSORS = "Sensor ids and names",
             BANDS = "Sensor ids and meta names for band information",
             RSR = "RSR ids and sensor names",
             L4_BANDS = "Band information for Landsat 4 bands",
             L5_BANDS = "Band information for Landsat 5 bands",
             L7_BANDS = "Band information  for Landsat 7 bands",
             L8_BANDS = "Band information  for Landsat 8 bands",
             L7_SRS = "Landat 7 relative spectral response (nm-1) taken from http://landsat.usgs.gov/instructions.php",
             L8_SRS = "Landat 8 relative spectral response (nm-1) taken from http://landsat.usgs.gov/instructions.php",
             SOLAR = "Solar irradiance (units: W m-2 nm-1) from the National Renewable Energy Laboratory taken from http://rredc.nrel.gov/solar/spectra/am0/modtran.html",
             L5_ESUN = "Tabulated ESun values from Chander and Markham (2003), tab. II, taken from http://landsathandbook.gsfc.nasa.gov/pdfs/L5TMLUTIEEE2003.pdf",
             L4_ESUN = "Tabulated ESun values from Chander and Markham (2003), tab. II, taken from http://landsathandbook.gsfc.nasa.gov/pdfs/L5TMLUTIEEE2003.pdf",
             L7_ESUN = "Tabulated ESun values from Landsat7 handbook, tab 11.3 (Thuillier SPECTRUM), taken from http://landsathandbook.gsfc.nasa.gov/pdfs/Landsat7_Handbook.pdf")
             
# Create sysdata.rda
lut <- list(SENSORS = sensors, 
            BANDS = bands,
            RSR = rsr,
            L4_BANDS = l4_bands, L5_BANDS = l5_bands, 
            L7_BANDS = l7_bands, L8_BANDS = l8_bands,
            L7_RSR = l7_rsr, L8_RSR = l8_rsr, SOLAR = solar, 
            L4_ESUN = l4_esun, L5_ESUN = l5_esun, L7_ESUN = l7_esun,
            META = meta)
devtools::use_data(lut, overwrite = TRUE, internal = TRUE)
