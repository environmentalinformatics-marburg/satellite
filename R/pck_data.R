# Package methods used to build datasets
#
# @description
# Functions which have been used to create dataset of this package.
# 
# @name pck_data
# 
NULL

# Dataset l7 -------------------------------------------------------------------
pck_data_l7 <- function(){
  fls <- list.files("inst/extdata", pattern = "^LE07.*.TIF", full.names = TRUE)
  lst <- lapply(fls, raster)
  ids <- sapply(lst, function(i) all(raster::res(i) == 30))
  l7 <- raster::stack(lst[ids])
  l7 <- raster::readAll(l7)
  devtools::use_data(l7, overwrite = TRUE)
}


# Dataset l8 -------------------------------------------------------------------
pck_data_l8 <- function(){
  fls <- list.files("inst/extdata", pattern = "^LC08.*.TIF", full.names = TRUE)
  fls <- sortFilesLandsat(fls)
  lst <- lapply(fls, raster::raster)
  ids <- sapply(lst, function(i) all(raster::res(i) == 30))
  l8 <- raster::stack(lst[ids])
  l8 <- raster::readAll(l8)
  devtools::use_data(l8, overwrite = TRUE)
}


# Datasets in inst/extdata -----------------------------------------------------
pck_data_extdata <- function(dsn = "E:/programming/r/satellite/data") {

  ## loop over landsat 8 (LC08*) and 7 (LE07*) scenes
  scenes <- c("LC08_L1TP_195025_20130707_20170503_01_T1", 
              "LE07_L1TP_195025_20010730_20170204_01_T1")
  
  for (scene in scenes) {
    
    # band files
    sid <- lutInfoSIDfromFilename(scene)
    fls <- list.files(file.path(dsn, scene),
                      pattern = paste0("^", sid, ".*.TIF"), full.names = TRUE)
    
    nms <- paste0("inst/extdata/", basename(fls))
    lst <- lapply(1:length(fls), function(i) {
      rst <- raster::raster(fls[i])
      raster::crop(rst, l8, filename = nms[i], datatype = raster::dataType(rst))
    }); rm(lst)
    
    jnk <- file.rename(gsub(".TIF$", ".tif", nms), nms)

    # metadata    
    mtd <- list.files(unique(dirname(fls)), pattern = "MTL.txt", full.names = TRUE)
    file.copy(mtd, paste0("inst/extdata/", basename(mtd)))
  }
}

# DEM in inst/extdata ------------------------------------------------------
pck_data_extdata_DEM <- function(dsn = "E:/programming/r/satellite/data") {
  path <- system.file("extdata", package = "satellite")
  files <- list.files(path, pattern = "^LC08.*.TIF$", full.names = TRUE)
  rst <- raster::raster(files[1])
  rst_ll <- raster::projectExtent(rst, crs = "+init=epsg:4326")

  len <- length(sp::coordinates(rst_ll)[, 1]) / 2
  lon <- sp::coordinates(rst_ll)[len, 1]
  lat <- sp::coordinates(rst_ll)[len, 2]
  dem_ll <- raster::getData('SRTM', lon = lon, lat = lat, path = dsn)
  
  nms <- "inst/extdata/DEM.TIF"
  dem <- raster::projectRaster(dem_ll, rst[[1]], filename = nms, 
                               datatype = raster::dataType(dem_ll))
  
  jnk <- file.rename(gsub(".TIF$", ".tif", nms), nms)
}

# Spatial extent of Georg-Gassmann-Stadion (used in Examples and during testing)
ext_ggs <- function() {
  ggs <- data.frame(y = 50.797761, x = 8.754611, loc = "Georg-Gassmann-Stadion")
  coordinates(ggs) <- ~ x + y
  proj4string(ggs) <- "+init=epsg:4326"
  
  ggs_utm <- spTransform(ggs, CRS("+init=epsg:32632"))
  crd_utm <- coordinates(ggs_utm)
  ext_utm <- extent(crd_utm[1] - 100, crd_utm[1] + 75, 
                    crd_utm[2] - 125, crd_utm[2] + 125)
  spy_utm <- as(ext_utm, "SpatialPolygons")
  proj4string(spy_utm) <- proj4string(ggs_utm)
  
  return(extent(spy_utm))
}
  