# devtools::test(".", "calcTOAIrrad")
context("Solar irradiation (ESun) using radiation vs. reflection")

#-------------------------------------------------------------------------------
test_that("calcTOAIrradRadRef for 'numeric' input works as expected", {
  path <- system.file("extdata", package = "satellite")
  files <- list.files(path, pattern = glob2rx("LC08*.TIF"), full.names = TRUE)
  sat <- satellite(files)  
  test <- calcTOAIrradRadRef(x = getSatRadMax(sat, getSatBCDESolar(sat)), 
                             ref_max = getSatRefMax(sat, getSatBCDESolar(sat)), 
                             normalize = FALSE,
                             esd = calcEarthSunDist("2013-07-07")) 
  
  expect_equal(round(as.numeric(test[1]), 3), round( 1907.999, 3))
  expect_equal(round(as.numeric(test[3]), 3), round( 1800.423, 3))
})

#-------------------------------------------------------------------------------
test_that("calcTOAIrradRadRef for 'numeric' input (P1L1) works as expected", {
  path <- system.file("testdata/LC8", package = "satellite")
  files <- list.files(path, pattern = glob2rx("LC8*.TIF"), full.names = TRUE)
  sat <- satellite(files)  
  test <- calcTOAIrradRadRef(x = getSatRadMax(sat, getSatBCDESolar(sat)), 
                             ref_max = getSatRefMax(sat, getSatBCDESolar(sat)), 
                             normalize = FALSE,
                             esd = calcEarthSunDist("2017-03-08")) 
})

#-------------------------------------------------------------------------------
test_that("calcTOAIrradRadRef for 'Satellite' input works as expected", {
  path <- system.file("extdata", package = "satellite")
  files <- list.files(path, pattern = glob2rx("LC08*.TIF"), full.names = TRUE)
  sat <- satellite(files)  
  test <- calcTOAIrradRadRef(sat)
  
  expect_equal(as.character(getSatBID(test)[1]), "1")
  expect_equal(round(as.numeric(getSatESUN(test)[1]), 3), round(  1972.254, 3))
  expect_equal(as.character(getSatBID(test)[3]), "3")
  expect_equal(round(as.numeric(getSatESUN(test)[3]), 3), round( 1861.055, 3))
  
  sat <- satellite(files[c(1,3,4)])
  test <- calcTOAIrradRadRef(sat)
  
  expect_equal(as.character(getSatBID(test)[1]), "1")
  expect_equal(round(as.numeric(getSatESUN(test)[1]), 3), round( 1972.254, 3))
  expect_equal(as.character(getSatBID(test)[2]), "2")
  expect_equal(round(as.numeric(getSatESUN(test)[2]), 3), round( 2019.612, 3))
})

#-------------------------------------------------------------------------------
test_that("calcTOAIrradRadRef for 'Satellite' input (P1L1) works as expected", {
  path <- system.file("testdata/LC8", package = "satellite")
  files <- list.files(path, pattern = glob2rx("LC8*.TIF"), full.names = TRUE)
  sat <- satellite(files)  
  test <- calcTOAIrradRadRef(sat)
  
  expect_equal(as.character(getSatBID(test)[1]), "1")
  expect_equal(round(as.numeric(getSatESUN(test)[1]), 3), round(  1972.254, 3))
  expect_equal(as.character(getSatBID(test)[3]), "3")
  expect_equal(round(as.numeric(getSatESUN(test)[3]), 3), round( 1861.055, 3))
  
  sat <- satellite(files[c(1,3,4)])
  test <- calcTOAIrradRadRef(sat)
  
  expect_equal(as.character(getSatBID(test)[1]), "1")
  expect_equal(round(as.numeric(getSatESUN(test)[1]), 3), round( 1972.254, 3))
  expect_equal(as.character(getSatBID(test)[2]), "2")
  expect_equal(round(as.numeric(getSatESUN(test)[2]), 3), round( 2019.612, 3))
})
