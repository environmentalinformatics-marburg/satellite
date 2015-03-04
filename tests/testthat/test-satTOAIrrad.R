context("satTOAIrrad")

test_that("satTOAIrrad for table works as expected", {
  path <- system.file("extdata", 
                      package = "satellite")
  files <- list.files(path, 
                      pattern = glob2rx("LE7*.tif"), 
                      full.names = TRUE)
  sat <- satellite(files)
  test <- satTOAIrrad(sat, method = "Table")
  
  expect_equal(as.character(getSatBID(test)[2]), "2")
  expect_equal(as.numeric(getSatESUN(test)[2]), 1812.0)
  
  sat <- satellite(files[c(1, 3, 6)])
  test <- satTOAIrrad(sat, method = "Table")
  
  expect_equal(as.character(getSatBID(test)[2]), "3")
  expect_equal(as.numeric(getSatESUN(test)[2]), 1533.0)
  
  path <- system.file("extdata", 
                      package = "satellite")
  files <- list.files(path, 
                      pattern = glob2rx("LC8*.tif"), 
                      full.names = TRUE)
  sat <- satellite(files)  
  expect_error(satTOAIrrad(sat, method = "Table"), 
               "Satellite ID LC8 is not supported, yet.")
})


test_that("satTOAIrrad for model works as expected", {
  path <- system.file("extdata", 
                      package = "satellite")
  files <- list.files(path, 
                      pattern = glob2rx("LC8*.tif"), 
                      full.names = TRUE)
  sat <- satellite(files)  
  test <- satTOAIrrad(sat, method = "Model")
  
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
  test <- satTOAIrrad(sat, method = "Model")
  
  expect_equal(as.character(getSatBID(test)[1]), "1")
  expect_equal(round(as.numeric(getSatESUN(test)[1]),4), round(1888.4115033, 4))
  expect_equal(as.character(getSatBID(test)[2]), "2")
  expect_equal(round(as.numeric(getSatESUN(test)[2]),4), round(1974.8429354, 4))
  expect_equal(as.character(getSatBID(test)[3]), "11")
  expect_equal(round(as.numeric(getSatESUN(test)[3]),4), round(0.1068904, 4))
})

test_that("satTOAIrrad for rad/ref works as expected", {
  path <- system.file("extdata", 
                      package = "satellite")
  files <- list.files(path, 
                      pattern = glob2rx("LC8*.tif"), 
                      full.names = TRUE)
  sat <- satellite(files)  
  test <- satTOAIrrad(sat, method = "RadRef")
  
  expect_equal(as.character(getSatBID(test)[1]), "1")
  expect_equal(round(as.numeric(getSatESUN(test)[1]), 3), round( 1907.999, 3))
  expect_equal(as.character(getSatBID(test)[3]), "3")
  expect_equal(round(as.numeric(getSatESUN(test)[3]), 3), round( 1800.423, 3))
  
  sat <- satellite(files[c(1,3,4)])
  test <- satTOAIrrad(sat, method = "RadRef")
  
  expect_equal(as.character(getSatBID(test)[1]), "1")
  expect_equal(round(as.numeric(getSatESUN(test)[1]), 3), round( 1907.999, 3))
  expect_equal(as.character(getSatBID(test)[2]), "2")
  expect_equal(round(as.numeric(getSatESUN(test)[2]), 3), round( 1953.814, 3))
})
