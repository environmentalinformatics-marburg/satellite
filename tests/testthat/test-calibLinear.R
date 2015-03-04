context("calibLinear")

test_that("calibLinear works as expected", {
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
