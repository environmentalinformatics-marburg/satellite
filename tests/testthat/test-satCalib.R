context("satCalib")

test_that("satCalib works as expected", {
  path <- system.file("extdata", 
                      package = "satellite")
  files <- list.files(path, 
                      pattern = glob2rx("LC8*.tif"), 
                      full.names = TRUE)
  sat <- satellite(files)
  
  t1 <- satCalib(sat, convert = "Rad")
  t2 <- satCalib(sat, convert = "Ref", szen_correction = TRUE)
  t3 <- satCalib(sat, convert = "Ref", szen_correction = FALSE)
  t4 <- satCalib(sat, convert = "BT")
  t5 <- satCalib(sat, convert = "all")
  
  expect_equal(
    round(raster::getValues(getSatDataLayer(t1, bcde = "B002n_RAD"))[50],4), 
    round(62.95932, 4))
  expect_equal(
    round(raster::getValues(getSatDataLayer(t2, bcde = "B002n_REF"))[50],4), 
    round( 0.1179185, 4))
  expect_equal(
    round(raster::getValues(getSatDataLayer(t3, bcde = "B002n_REF_NoSZEN"))[50],4), 
    round(0.10124, 4))
  expect_equal(
    round(raster::getValues(getSatDataLayer(t4, bcde = "B010n_BT"))[50],4), 
    round(303.5775, 4))
  expect_equal(
    round(raster::getValues(getSatDataLayer(t5, bcde = "B009n_RAD"))[50],4), 
    round(0.1295196, 4))
  })
