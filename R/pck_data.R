#' Package methods used to build datasets
#'
#' @description
#' Functions which have been used to create dataset of this package.
#' 
#' @name pck_data
#' 
NULL

# Dataset l7 -------------------------------------------------------------------
pck_data_l7 <- function(){
  l7 <- raster::stack("inst/extdata/l7_2001-07-30_30m_crop_R_satellite.tif")
  devtools::use_data(l7, overwrite = TRUE)
}


# Dataset l8 -------------------------------------------------------------------
pck_data_l8 <- function(){
  l8 <- raster::stack("inst/extdata/l8_2013-07-07_30m_crop_R_satellite.tif")
  devtools::use_data(l8, overwrite = TRUE)
}


# Datasets in inst/exdata ------------------------------------------------------
pck_data_exdata <- function(){
  l8 <- raster::stack("inst/extdata/l8_2013-07-07_30m_crop_R_satellite.tif")
  mask <- l8[[2]]
  inpath <- "D:/active/moc/am-remote-sensing/examples/data/landsat/l8_2013-07-07"
  files <- list.files(inpath, pattern = glob2rx("*.TIF"), full.names = TRUE)
  lapply(files, function(x){
    print(x)
    writeRaster(crop(raster(x), mask), 
                filename = paste0("inst/extdata/",basename(x)), format = "GTiff")
  })
  
  inpath <- "D:/active/moc/am-remote-sensing/examples/data/landsat/l7_2001-07-30"
  files <- list.files(inpath, pattern = glob2rx("*.TIF"), full.names = TRUE)
  lapply(files, function(x){
    print(x)
    writeRaster(crop(raster(x), mask), 
                filename = paste0("inst/extdata/",basename(x)), format = "GTiff")
  })
}
