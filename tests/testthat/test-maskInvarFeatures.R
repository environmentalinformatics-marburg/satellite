context("maskInvarFeatures")

test_that("maskInvarFeatures works as expected", {
  path <- system.file("testdata/LC8", 
                      package = "satellite")
  files <- list.files(path, 
                      pattern = glob2rx("LC8*.TIF"), 
                      full.names = TRUE)
  sat <- satellite(files)

  t <- maskInvarFeatures(x = getSatDataLayer(sat, "B004n"), 
                    nir = getSatDataLayer(sat, "B005n"), 
                    swir = getSatDataLayer(sat, "B007n"))
})






