# devtools::test(".", "calcTOAIrradTable")
context("Solar irradiation (ESun) using tabulated values")

#-------------------------------------------------------------------------------
test_that("calcTOAIrradTable for 'character' input works as expected", {
  calcTOAIrradTable(x = "LT5")
  calcTOAIrradTable(x = "LE7")
  calcTOAIrradTable(x = "LE7", normalize = FALSE, 
                    esd = calcEarthSunDist("2015-01-01"))
  expect_error(calcTOAIrradTable(x = "GLS5"), "Satellite ID GLS5 is not supported, yet.")
})


#-------------------------------------------------------------------------------
test_that("calcTOAIrradTable for 'Satellite' input works as expected", {
  path <- system.file("extdata", package = "satellite")
  files <- list.files(path, pattern = glob2rx("LE7*.TIF"), full.names = TRUE)
  sat <- satellite(files)
  test <- calcTOAIrradTable(sat)
  
  expect_equal(as.character(getSatBID(test)[2]), "2")
  expect_equal(as.numeric(getSatESUN(test)[2]), 1842)
  
  sat <- satellite(files[c(1, 3, 6)])
  test <- calcTOAIrradTable(sat)
  
  expect_equal(as.character(getSatBID(test)[2]), "3")
  expect_equal(as.numeric(getSatESUN(test)[2]), 1547)
  
  path <- system.file("extdata", package = "satellite")
  files <- list.files(path, pattern = glob2rx("LC8*.TIF"), full.names = TRUE)
  sat <- satellite(files)  
  expect_error(calcTOAIrradTable(sat))
})
