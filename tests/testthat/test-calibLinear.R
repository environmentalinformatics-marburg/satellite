context("calibLinear")

test_that("calibLinear works as expected for Satellite", {
  path <- system.file("extdata", 
                      package = "satellite")
  files <- list.files(path, 
                      pattern = glob2rx("LC8*.tif"), 
                      full.names = TRUE)
  sat <- satellite(files)
  
  t1 <- calibLinear(sat, convert = "Rad")
  t2 <- calibLinear(sat, convert = "Ref", szen_correction = TRUE)
  t3 <- calibLinear(sat, convert = "Ref", szen_correction = FALSE)
  t4 <- calibLinear(sat, convert = "BT")
  t5 <- calibLinear(sat, convert = "all")
  
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



test_that("calibLinear works as expected for Raster", {
  path <- system.file("extdata", 
                      package = "satellite")
  files <- list.files(path, 
                      pattern = glob2rx("LC8*.tif"), 
                      full.names = TRUE)
  sat <- satellite(files)
  
  bcde <- "B002n"
  
  t1 <- calibLinear(x = getValues(getSatDataLayer(sat, bcde)),
                    bnbr = 1,
                    mult = getSatREFM(sat, bcde),
                    add = getSatREFA(sat, bcde))
  
  t2 <- calibLinear(band = getSatDataLayer(sat, bcde),
                    bnbr = 1,
                    mult = getSatREFM(sat, bcde),
                    add = getSatREFA(sat, bcde),
                    szen = getSatSZEN(sat, bcde))
  
  t3 <- calibLinear(band = getSatDataLayer(sat, bcde),
                    bnbr = 1,
                    mult = getSatRADM(sat, bcde),
                    add = getSatRADA(sat, bcde))

  bcde <- "B010n"
  t4 <- calibLinear(band = getSatDataLayer(sat, bcde),
                    bnbr = 1,
                    mult = getSatRADM(sat, bcde),
                    add = getSatRADA(sat, bcde),
                    k1 = getSatBTK1(sat, bcde),
                    k2 = getSatBTK2(sat, bcde))
  
  expect_equal(round(raster::getValues(t1)[50],3), round(0.10124,3))
  expect_equal(round(raster::getValues(t2)[50],3), round(0.1179185,3))
  expect_equal(round(raster::getValues(t3)[50],3), round(62.959,3))
  expect_equal(round(raster::getValues(t4)[50],3), round(303.5775,3))
})
