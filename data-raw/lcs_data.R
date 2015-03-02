lcs <- rgdal::readOGR("data-raw/l8_2013-07-07_30m_crop_R_satellite_landcover_samples.shp", 
                      layer = "l8_2013-07-07_30m_crop_R_satellite_landcover_samples")
devtools::use_data(lcs, overwrite = TRUE)
