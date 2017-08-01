context("maskInvarFeatures")

test_that("maskInvarFeatures works as expected", {
  path <- system.file("testdata/LC8", 
                      package = "satellite")
  files <- list.files(path, 
                      pattern = glob2rx("LC8*.TIF"), 
                      full.names = TRUE)
  sat <- satellite(files)
  t1 <- maskInvarFeatures(sat)
  
  expect_equal(
    raster::getValues(getSatDataLayer(t1, bcde = "M0000_InvarFeatures"))[77],
    TRUE)
  })
