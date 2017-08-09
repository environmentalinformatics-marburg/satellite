# devtools::test(".", "calcTOAIrradTable")
context("Solar irradiation (ESun) using tabulated values")

#-------------------------------------------------------------------------------
test_that("calcTOAIrradTable for 'character' input works as expected", {
  for (sid in c("LT4", "LT04")) 
    expect_equal(calcTOAIrradTable(x = sid), lut$L4_ESUN)
  for (sid in c("LT5", "LT05")) 
    expect_equal(calcTOAIrradTable(x = sid), lut$L5_ESUN)
  for (sid in c("LE7", "LE07")) 
    expect_equal(calcTOAIrradTable(x = sid), lut$L7_ESUN)
  for (sid in c("LC8", "LC08"))
    expect_error(calcTOAIrradTable(x = sid), "ESun values are not provided")
  
  expect_error(calcTOAIrradTable(x = "GLS5"), "GLS5 is not supported, yet.")
  
  expect_is(calcTOAIrradTable(x = "LE7", normalize = FALSE, 
                              esd = calcEarthSunDist("2015-01-01")), "numeric")
})


#-------------------------------------------------------------------------------
test_that("calcTOAIrradTable for 'Satellite' input works as expected", {
  path <- system.file("testdata/LE7", package = "satellite")
  files <- list.files(path, pattern = glob2rx("LE7*.TIF"), full.names = TRUE)
  sat <- satellite(files)
  test <- calcTOAIrradTable(sat)
  
  expect_equal(as.character(getSatBID(test)[2]), "2")
  expect_equal(as.numeric(getSatESUN(test)[2]), 1842)
  
  sat <- satellite(files[c(1, 3, 6)])
  test <- calcTOAIrradTable(sat)
  
  expect_equal(as.character(getSatBID(test)[2]), "3")
  expect_equal(as.numeric(getSatESUN(test)[2]), 1547)
  
  path <- system.file("testdata/LC8", package = "satellite")
  files <- list.files(path, pattern = glob2rx("LC8*.TIF"), full.names = TRUE)
  sat <- satellite(files)  
  expect_error(calcTOAIrradTable(sat))
})
