l8 <- raster::stack("data-raw/l8_2013-07-07_30m_crop_R_satellite.tif")
names(l8) <- paste0(substr(names(l8), 1, 13), substr(names(l8), 35, nchar(names(l8))))
devtools::use_data(l8, overwrite = TRUE)
