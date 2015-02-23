# Create sysdata.rda

# Sensor names
sensors <- c(LC4 = "Landsat 4", LC5 = "Landsat 5", LC7 = "Landsat 7", 
             LC8 = "Landsat 8")

# Landat 7 relative spectral response (units: nm-1)
l7_rsr <- read.table("data-raw/landsat_7_relative_spectral_response.csv",
                     header = TRUE, sep = ";")

# Landat 8 relative spectral response (units: nm-1)
l8_rsr <- read.table("data-raw/landsat_8_relative_spectral_response.csv",
                     header = TRUE, sep = ";")

# Band wavelengths, bandwith data taken from
# http://landsat.usgs.gov/band_designations_landsat_satellites.php
l4_band_wl <- data.frame(lmin = c(0.45, 0.52, 0.63, 0.76, 1.55, 10.40, 2.08),
                         lmax = c(0.52, 0.60, 0.69, 0.90, 1.75, 12.50, 2.35))
rownames(l5_band_wl) <- names(eSun) <- paste0("Band_", seq(nrow(l4_band_wl)))

l5_band_wl <- data.frame(lmin = c(0.45, 0.52, 0.63, 0.76, 1.55, 10.40, 2.08),
                         lmax = c(0.52, 0.60, 0.69, 0.90, 1.75, 12.50, 2.35))
rownames(l5_band_wl) <- names(eSun) <- paste0("Band_", seq(nrow(l5_band_wl)))

l7_band_wl <- data.frame(
  lmin = c(0.45, 0.52, 0.63, 0.77, 1.55, 10.40, 2.09, 0.52),
  lmax = c(0.52, 0.60, 0.69, 0.90, 1.75, 12.50, 2.35, 0.90))
rownames(l7_band_wl) <- names(eSun) <- paste0("Band_", seq(nrow(l7_band_wl)))

l8_band_wl <- data.frame(
  lmin = c(0.43, 0.45, 0.53, 0.64, 0.85, 1.57, 2.11, 0.50, 1.36, 10.60, 11.50),
  lmax = c(0.45, 0.51, 0.59, 0.67, 0.88, 1.65, 2.29, 0.68, 1.38, 11.19, 12.51))
rownames(l8_band_wl) <- paste0("Band_", seq(nrow(l8_band_wl)))


# Solar irradiance (units: W m-2 nm-1)
solar <- read.table("data-raw/mod_etr_solar_irradiance.csv",
                  header = TRUE, sep = ";")

# Tabulated values of ESun (W m-2 micrometer-1)
l7_esun <- c(1997, 1812, 1533, 1039, 230.8, NA, 84.90, 1362)
names(l7_esun) <- paste0("Band_", seq(length(l7_esun)))

l5_esun <- c(1957, 1825, 1557, 1033, 214.9, NA, 80.72)
names(l5_esun) <- paste0("Band_", seq(length(l5_esun)))

l4_esun <- c(1957, 1826, 1554, 1036, 215, NA, 80.67)
names(l4_esun) <- paste0("Band_", seq(length(l4_esun)))

meta <- list(sensors = "Sensor ids and names",
             l4_band_wl = "Minimum/maximum wavelength for Landsat 4 bands",
             l5_band_wl = "Minimum/maximum wavelength for Landsat 5 bands",
             l7_band_wl = "Minimum/maximum wavelength for Landsat 7 bands",
             l8_band_wl = "Minimum/maximum wavelength for Landsat 8 bands",
             l7_rsr = "Landat 7 relative spectral response (nm-1) taken from http://landsat.usgs.gov/instructions.php",
             l8_rsr = "Landat 8 relative spectral response (nm-1) taken from http://landsat.usgs.gov/instructions.php",
             solar = "Solar irradiance (units: W m-2 nm-1) from the National Renewable Energy Laboratory taken from http://rredc.nrel.gov/solar/spectra/am0/modtran.html",
             l7_esun = "Tabulated ESun values from Landsat7 handbook, tab 11.3 (Thuillier spectrum), taken from http://landsathandbook.gsfc.nasa.gov/pdfs/Landsat7_Handbook.pdf",
             l5_esun = "Tabulated ESun values from Chander and Markham (2003), tab. II, taken from http://landsathandbook.gsfc.nasa.gov/pdfs/L5TMLUTIEEE2003.pdf",
             l4_esun = "Tabulated ESun values from Chander and Markham (2003), tab. II, taken from http://landsathandbook.gsfc.nasa.gov/pdfs/L5TMLUTIEEE2003.pdf")


# Create sysdata.rda
lut <- list(sensors = sensors, 
            l4_band_wl = l4_band_wl, l5_band_wl = l5_band_wl, 
            l7_band_wl = l7_band_wl, l8_band_wl = l8_band_wl,
            l7_rsr = l7_rsr, l8_rsr = l8_rsr, solar = solar, 
            l4_esun = l4_esun, l5_esun = l5_esun, l7_esun = l7_esun,
            meta = meta)
devtools::use_data(lut, overwrite = TRUE, internal = TRUE)
