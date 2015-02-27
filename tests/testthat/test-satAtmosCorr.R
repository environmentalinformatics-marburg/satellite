context("satAtmosCorr")

test_that("satAtmosCorr works as expected", {
  path <- system.file("extdata", 
                      package = "satellite")
  files <- list.files(path, 
                      pattern = glob2rx("LC8*.tif"), 
                      full.names = TRUE)
  sat <- satellite(files)
  atmos_corr <- satAtmosCorr(sat, atmos_model = "DOS2", esun_mode = "RadRef")
})
