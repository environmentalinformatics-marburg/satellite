library(raster)

l7 <- stack("data-raw/l7_2001-07-30_30m_crop_R_satellite.tif")

devtools::use_data(l7)
