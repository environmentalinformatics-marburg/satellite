context("pathRadianceDOS")

test_that("pathRadianceDOS works as expected", {
#   landsat7_metadatafile <- system.file("extdata", 
#                                        "LE71950252001211EDC00_MTL.txt", 
#                                        package = "satellite")
  landsat8_metadatafile <-   system.file("extdata", 
                                         "LC81950252013188LGN00_MTL.txt", 
                                         package = "satellite")
  
  # coefs7 <- landsatCoefficients(landsat7_metadatafile)
  coefs8 <- landsatCoefficients(landsat8_metadatafile)
  
  pathRadianceDOS(sensor = "Landsat 8", DNmin = min(raster::getValues(l8)), 
                  bnbr = 1, coefs = coefs8, date = "2013-07-30")
})

