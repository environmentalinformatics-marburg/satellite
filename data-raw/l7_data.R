l7 <- raster::stack("data-raw/l7_2001-07-30_30m_crop_R_satellite.tif")
names(l7) <- paste0(substr(names(l7), 1, 13), substr(names(l7), 35, nchar(names(l7))))
devtools::use_data(l7, overwrite = TRUE)
