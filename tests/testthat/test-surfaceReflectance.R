context("pathRadianceDOS")

test_that("pathRadianceDOS works as expected", {
#   landsat7_metadatafile <- system.file("extdata", 
#                                        "LE71950252001211EDC00_MTL.txt", 
#                                        package = "satellite")
  landsat8_metadatafile <-   system.file("extdata", 
                                         "LC81950252013188LGN00_MTL.txt", 
                                         package = "satellite")
  
  # coefs7 <- landsatMetadata(landsat7_metadatafile)
  coefs8 <- landsatMetadata(landsat8_metadatafile)
  
  ESun <-  eSun(sensor = "Landsat 8", tab = TRUE, coefs = coefs8)
  L0 <- pathRadianceDOS(sensor = "Landsat 8", 
                        DNmin = min(raster::getValues(l8[[2]])), 
                        bnbr = 2, coefs = coefs8, ESun = ESun, scat_coef = -4)
  
  bands <- l8
  for(i in seq(raster::nlayers(bands))){
    bands[[i]] <- landsatCalibration(bands[[i]], bnbr = i, coefs = coefs8)
  }
  
  sr <- surfaceReflectance(bands_rad = bands, coefs = coefs8, ESun = ESun, Lp = Lp)
  
})

