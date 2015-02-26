context("satAtmosCorr")

test_that("satAtmosCorr works as expected", {
  path <- system.file("extdata", 
                      package = "satellite")
  files <- list.files(path, 
                      pattern = glob2rx("LC8*.tif"), 
                      full.names = TRUE)
  sat <- satellite(files)
  test <- satAtmosCorr(sat)
  
  expect_equal(as.character(getSatBID(test)[2]), "2")
  expect_equal(as.numeric(getSatESUN(test)[2]), 1812.0)
  
  sat <- satellite(files[c(1, 3, 6)])
  test <- satTOAIrradTable(sat)
  
  expect_equal(as.character(getSatBID(test)[2]), "3")
  expect_equal(as.numeric(getSatESUN(test)[2]), 1533.0)
  
  path <- system.file("extdata", 
                      package = "satellite")
  files <- list.files(path, 
                      pattern = glob2rx("LC8*.tif"), 
                      full.names = TRUE)
  sat <- satellite(files)  
  expect_error(satTOAIrradTable(sat), "Satellite ID LC8 is not supported, yet.")
})
