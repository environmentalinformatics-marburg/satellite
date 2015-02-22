context("landsatCalibration")

test_that("landsatCalibration works as expected", {
  landsat7_metadatafile <- system.file("extdata", 
                                       "LE71950252001211EDC00_MTL.txt", 
                                       package = "satellite")
  landsat8_metadatafile <-   system.file("extdata", 
                                         "LC81950252013188LGN00_MTL.txt", 
                                         package = "satellite")
  
  # coefs7 <- landsatMetadata(landsat7_metadatafile)
  coefs8 <- landsatMetadata(landsat8_metadatafile)
  
  expect_equal(round(raster::getValues(
    landsatCalibration(l8[[2]], 2, coefs8, conv = "ref"))[50],3), 
    round(0.10124,3))
  expect_equal(round(raster::getValues(
    landsatCalibration(l8[[2]], 2, coefs8, conv = "refsun"))[50],3), 
    round(0.1179185,3))
  expect_equal(round(raster::getValues(
    landsatCalibration(l8[[10]], 10, coefs8, conv = "bt"))[50],3), 
    round(296.3723,3))
  expect_error(landsatCalibration(l8[[2]], 2, coefs8, conv = "bt"),
               "Missing temperature correction factors for band")
})
