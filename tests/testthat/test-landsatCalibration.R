context("landsatCalibration")

test_that("landsatCalibration works as expected", {
  path <- system.file("extdata", 
                      package = "satellite")
  filesl8 <- list.files(path, 
                       pattern = glob2rx("LC8*.tif"), 
                       full.names = TRUE)
  
  coefs8 <- collectLandsat8Metadata(filesl8)
  
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
