l8 <- raster::stack("data-raw/l8_2013-07-07_30m_crop_R_satellite.tif")

devtools::use_data(l8, overwrite = TRUE)
