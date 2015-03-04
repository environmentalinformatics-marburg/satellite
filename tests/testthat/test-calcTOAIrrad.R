context("calcTOAIrrad")

test_that("calcTOAIrrad works as expected for Satellite", {
  path <- system.file("extdata", 
                      package = "satellite")
  files <- list.files(path, 
                      pattern = glob2rx("LE7*.tif"), 
                      full.names = TRUE)
  sat <- satellite(files)
  test <- calcTOAIrrad(sat, method = "Table")
  
  expect_equal(as.character(getSatBID(test)[2]), "2")
  expect_equal(as.numeric(getSatESUN(test)[2]), 1812.0)
  
  sat <- satellite(files[c(1, 3, 6)])
  test <- calcTOAIrrad(sat, method = "Table")
  
  expect_equal(as.character(getSatBID(test)[2]), "3")
  expect_equal(as.numeric(getSatESUN(test)[2]), 1533.0)
  
  path <- system.file("extdata", 
                      package = "satellite")
  files <- list.files(path, 
                      pattern = glob2rx("LC8*.tif"), 
                      full.names = TRUE)
  sat <- satellite(files)  
  
  expect_error(calcTOAIrrad(sat, method = "Table"), 
               "Satellite ID LC8 is not supported, yet.")

  path <- system.file("extdata", 
                      package = "satellite")
  files <- list.files(path, 
                      pattern = glob2rx("LC8*.tif"), 
                      full.names = TRUE)
  sat <- satellite(files) 
  
  test <- calcTOAIrrad(sat, method = "Model")
  
  expect_equal(as.character(getSatBID(test)[1]), "1")
  expect_equal(round(as.numeric(getSatESUN(test)[1]),4), round(1888.4115033, 4))
  expect_equal(as.character(getSatBID(test)[2]), "2")
  expect_equal(round(as.numeric(getSatESUN(test)[2]),4), round(1974.8429354, 4))
  expect_equal(as.character(getSatBID(test)[3]), "3")
  expect_equal(round(as.numeric(getSatESUN(test)[3]),4), round(1851.7520559, 4))
  expect_equal(as.character(getSatBID(test)[11]), "11")
  expect_equal(round(as.numeric(getSatESUN(test)[11]),4), round(0.1068904, 4))
  
  path <- system.file("extdata", 
                      package = "satellite")
  files <- list.files(path, 
                      pattern = glob2rx("LC8*.tif"), 
                      full.names = TRUE)
  sat <- satellite(files[c(1,3,4)])  
  
  test <- calcTOAIrrad(sat, method = "Model")
  
  expect_equal(as.character(getSatBID(test)[1]), "1")
  expect_equal(round(as.numeric(getSatESUN(test)[1]),4), round(1888.4115033, 4))
  expect_equal(as.character(getSatBID(test)[2]), "2")
  expect_equal(round(as.numeric(getSatESUN(test)[2]),4), round(1974.8429354, 4))
  expect_equal(as.character(getSatBID(test)[3]), "11")
  expect_equal(round(as.numeric(getSatESUN(test)[3]),4), round(0.1068904, 4))

  path <- system.file("extdata", 
                      package = "satellite")
  files <- list.files(path, 
                      pattern = glob2rx("LC8*.tif"), 
                      full.names = TRUE)
  sat <- satellite(files)  
  
  test <- calcTOAIrrad(sat, method = "RadRef")
  
  expect_equal(as.character(getSatBID(test)[1]), "1")
  expect_equal(round(as.numeric(getSatESUN(test)[1]), 3), round( 1907.999, 3))
  expect_equal(as.character(getSatBID(test)[3]), "3")
  expect_equal(round(as.numeric(getSatESUN(test)[3]), 3), round( 1800.423, 3))
  
  path <- system.file("extdata", 
                      package = "satellite")
  files <- list.files(path, 
                      pattern = glob2rx("LC8*.tif"), 
                      full.names = TRUE)
  sat <- satellite(files[c(1,3,4)])
  
  test <- calcTOAIrrad(sat, method = "RadRef")
  
  expect_equal(as.character(getSatBID(test)[1]), "1")
  expect_equal(round(as.numeric(getSatESUN(test)[1]), 3), round( 1907.999, 3))
  expect_equal(as.character(getSatBID(test)[2]), "2")
  expect_equal(round(as.numeric(getSatESUN(test)[2]), 3), round( 1953.814, 3))
})


test_that("alcTOAIrrad works as expected for numeric", {
  calcTOAIrradModel(lut$L8_RSR, model = "MNewKur")
  calcTOAIrradModel(lut$L7_RSR, model = "MNewKur")
  calcTOAIrradModel(lut$L7_RSR, model = "MNewKur", normalize = FALSE, 
                    esd = calcEartSunDist("2015-01-01"))
})

test_that("calcTOAIrradRadTable works as expected for character", {
  calcTOAIrradRadTable(sid = "LE5")
  calcTOAIrradRadTable(sid = "LE7")
  calcTOAIrradRadTable(sid = "LE7", normalize = FALSE, 
                       esd = calcEartSunDist("2015-01-01"))
})
